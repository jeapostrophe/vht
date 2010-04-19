#lang scheme
(require scheme/file)

(define-struct handle (path) #:prefab)
(define-struct (resource handle) () #:prefab)
(define-struct (container handle) () #:prefab)

(define (create-resource parent v #:template [template "~a"])
  (define tmp-file (make-temporary-file))
  (with-output-to-file tmp-file
    (lambda () (write v))
    #:exists 'truncate/replace)
  (make-resource (path->string (make-temporary-file template tmp-file (handle-path parent)))))
(define (create-container parent #:template [template "~a"])
  (make-container (path->string (make-temporary-file template 'directory (handle-path parent)))))
(define (make-root path)
  (make-directory* path)
  (make-container (path->string path)))

(define (container-handles c)
  (define cp (handle-path c))
  (for/list ([p (in-list (directory-list cp))])
    (define ap (build-path cp p))
    (define aps (path->string ap))
    (if (directory-exists? ap)
        (make-container aps)
        (make-resource aps))))
(define (container-resources c)
  (filter resource? (container-handles c)))
(define (container-containers c)
  (filter container? (container-handles c)))

(define (template-string? s)
  (and (string? s)
       (= 1 (length (regexp-match* #rx"~a" s)))))

(define (resource-read* r)
  (with-handlers ([exn:fail? (lambda (x) #f)])
    (resource-read r)))
(define (resource-read r)
    (file->value (handle-path r)))

(define (read/write/c x)
  (with-handlers ([exn:fail? (lambda (x) #f)])
    (define b (open-output-bytes))
    (write x b)
    (let ([ib (open-input-bytes (get-output-bytes b))])
      (read ib))
    #t))

(provide/contract
 [handle? (any/c . -> . boolean?)]
 [handle-path (handle? . -> . string?)]
 [resource? (any/c . -> . boolean?)]
 [container? (any/c . -> . boolean?)]
 [rename create-resource make-resource
         ((container? read/write/c) (#:template template-string?) . ->* . resource?)]
 [rename create-container make-container
         ((container?) (#:template template-string?) . ->* . container?)]
 [make-root (path? . -> . container?)]
 [template-string? (any/c . -> . boolean?)]
 [container-handles (container? . -> . (listof handle?))]
 [container-resources (container? . -> . (listof resource?))]
 [container-containers (container? . -> . (listof container?))]
 [resource-read (resource? . -> . read/write/c)]
 [resource-read* (resource? . -> . (or/c read/write/c false/c))])