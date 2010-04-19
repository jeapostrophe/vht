#lang scheme
(require (for-syntax syntax/parse
                     scheme/list
                     unstable/syntax)
         "handle.ss"
         "versioned-ht.ss")

(define-syntax (define-vht-struct stx)
  (syntax-parse 
   stx
   [(_ struct:id
       (container:expr template:expr)
       ([field:id field/c:expr]
        ...))
    (with-syntax ([(set-struct-field!* ...)
                   (generate-temporaries #'(field ...))]
                  [(set-struct-field! ...)
                   (syntax-map (lambda (f) (format-id f "set-~a-~a!" #'struct f))
                               #'(field ...))]
                  [(field-kw ...)
                   (syntax-map (lambda (f) 
                                 (string->keyword
                                  (symbol->string
                                   (syntax-e f))))
                               #'(field ...))]
                  [(struct-field* ...)
                   (generate-temporaries #'(field ...))]
                  [(struct-field ...)
                   (syntax-map (lambda (f) (format-id f "~a-~a" #'struct f))
                               #'(field ...))]
                  [struct?
                   (format-id #'struct "~a?" #'struct)]
                  [make-struct
                   (format-id #'struct "make-~a" #'struct)])
      (quasisyntax/loc stx
        (begin
          (define the-container container)
          (define the-template template)
          (define struct-tag 'struct)
          
          (define (struct? x)
            (and (versioned-ht? x)
                 (eq? (dict-ref x 'type #f)
                      struct-tag)))
          
          (define (make-struct* #,@(flatten (syntax-map cons 
                                                        #'(field-kw ...)
                                                        #'(field ...))))
            (define vht
              (make-versioned-ht (make-container (the-container) #:template the-template)))
            (dict-set! vht 'type struct-tag)
            (set-struct-field!* vht field)
            ...
            vht)
          (define make-struct
            (contract (-> #,@(flatten (syntax-map cons 
                                                  #'(field-kw ...)
                                                  #'(field/c ...)))
                          struct?) make-struct*
                                   'module 'struct))
          
          (define (set-struct-field!* vht v)
            (dict-set! vht 'field v))
          ...
          (define set-struct-field!
            (contract (-> struct? field/c void) set-struct-field!*
                      'module 'struct))
          ...
          
          (define (struct-field* vht)
            (dict-ref vht 'field))
          ...
          (define struct-field
            (contract (-> struct? field/c) struct-field*
                      'module 'struct))
          ...
          
          (define-struct struct (field ...)
            #:omit-define-values))))]))

(provide define-vht-struct)