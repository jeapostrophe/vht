#lang scheme
(require tests/eli-tester
         "../handle.ss"
         "../versioned-ht.ss")

(define r (make-root "root"))
(define c (make-container r))
(define vht1 (make-versioned-ht c))
(define vht2 (make-versioned-ht c))

(test
 (dict-ref vht1 'foo #f) => #f
 (dict-set! vht1 'foo 'bar) => (void)
 (dict-ref vht1 'foo) => 'bar
 
 (dict-ref vht2 'foo) => 'bar
 
 (delete-directory/files (handle-path r)))