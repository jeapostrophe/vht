#lang scheme
(require tests/eli-tester
         "../handle.ss")

(test
 (template-string? "~a")
 (template-string? "~a.txt")
 (template-string? "foo~a.txt")
 (template-string? "~a~a") => #f
 (template-string? "~") => #f
 (template-string? "a~") => #f
 (template-string? 1) => #f
 (template-string? #"~a~a") => #f
 (template-string? #"~a") => #f
 
 (local [(define r (make-root "root"))]
   (test
    (resource? (make-resource r (random)))
    (resource? (make-resource r #:template "example~a" (random)))
    (local [(define c (make-container r #:template "user~a"))]
      (test
       (container? c)
       (resource? (make-resource c (random)))))
    (delete-directory/files (handle-path r))))
 
 
 (local [(define r (make-root "root"))
         (define rrs
           (for/list ([i (in-range 100)]) (random)))]
   (define rs 
     (for/list ([i (in-range 100)]
                [rr (in-list rrs)])
       (make-resource r rr)))
   (test
    (for ([r (in-list (container-resources r))]
          [rr (in-list rrs)])
      (test (member r rs)
            (member (resource-read r) rrs)))
    (delete-directory/files (handle-path r)))))