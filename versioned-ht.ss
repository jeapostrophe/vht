#lang scheme
(require "handle.ss")

(define-syntax-rule (define-vht/dict-bridge vht-ref dict-ref)
  (define (vht-ref vht . args)
    (unless (vht-last-read vht)
      (vht-update! vht))
    (apply dict-ref (vht-last-val vht) args)))
(define-syntax-rule (define-vht/dict-bridges [vht-ref dict-ref] ...)
  (begin (define-vht/dict-bridge vht-ref dict-ref) ...))

(define-vht/dict-bridges 
  [vht-ref dict-ref]
  [vht-count dict-count]
  [vht-iterate-first dict-iterate-first]
  [vht-iterate-next dict-iterate-next]
  [vht-iterate-key dict-iterate-key]
  [vht-iterate-value dict-iterate-value])

(define (make-versioned-ht parent)
  (make-vht parent #f #f))

(define-struct vht-log (stamp) #:prefab)
(define-struct (log-set! vht-log) (k v) #:prefab)
(define-struct (log-remove! vht-log) (k) #:prefab)

(define (vht-log-apply! l ht)
  (match l
    [(struct log-set! (_ k v)) (hash-set! ht k v)]
    [(struct log-remove! (_ k)) (hash-remove! ht k)]))

(define (vht-update! the-vht)
  (match-define (struct vht (c last-read last-val)) the-vht)
  (define new-val
    (or last-val (make-hash)))
  (define rs (container-resources c))
  (define ls (map resource-read rs))
  (define sls (sort ls < #:key vht-log-stamp))
  (define new-read
    (for/fold ([new-time (or last-read -inf.0)])
      ([l (in-list sls)])
      (define t (vht-log-stamp l))
      (when (t . > . new-time)
        (vht-log-apply! l new-val))
      (max new-time t)))
  (set-vht-last-read! the-vht new-read)
  (set-vht-last-val! the-vht new-val))

(define (vht-set! vht k v)
  (make-resource (vht-container vht)
                 (make-log-set! (current-inexact-milliseconds) k v))
  (vht-update! vht))
(define (vht-remove! vht k)
  (make-resource (vht-container vht)
                 (make-log-remove! (current-inexact-milliseconds) k))
  (vht-update! vht))

(define-struct vht (container [last-read #:mutable] [last-val #:mutable])
  #:property prop:dict
  (vector vht-ref
          vht-set! #f
          vht-remove! #f
          vht-count
          vht-iterate-first
          vht-iterate-next
          vht-iterate-key
          vht-iterate-value))

(provide/contract
 [rename vht? versioned-ht?
         (any/c . -> . boolean?)]
 [make-versioned-ht (container? . -> . vht?)]
 [rename vht-update! versioned-ht-update!
         (vht? . -> . void)])