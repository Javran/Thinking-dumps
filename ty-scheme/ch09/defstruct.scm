; get the position of obj in the list
(define list-position
  ; the function accepts 2 arg: obj and ls
  (lambda (obj ls)
    ; initialize index to 0, list(l) to ls
    (let loop ((i 0) (l ls))
      (if (null? l) 
        #f ; the list is empty, nothing can be found
        (if (eqv? (car l) obj) i ; ok, we've found the obj in list
          (loop (+ i 1) (cdr l)))))))

(define-syntax defstruct
  (rsc-macro-transformer 
    (let ((xfmr 
      ; defstruct accepts structure name & field names
      (lambda (s . ff)
        (let (
              ; keep structure name as string s-s
              (s-s (symbol->string s)) 
              ; keep field count
              (n (length ff)))

          (let* (
                  (n+1 (+ n 1))
                  (vv (make-vector n+1)))
            (let loop ((i 1) (ff ff))
              (if (<= i n)
                ; take the first element from field name list
                (let ((f (car ff)))
                  (vector-set! vv i 
                    (if (pair? f)
                      ; the constructor has default value for this field
                      ;   so we extract it from the pair
                      (cadr f) 
                      ; or, we'll just simply set it as 'undefined'(denoted by (if #f #f))
                      '(if #f #f)))
                  ; walk through all fields, store default values for them
                  (loop (+ i 1) (cdr ff)))))

            (let ((ff (map (lambda (f) (if (pair? f) (car f) f))
                       ff)))
              `(begin

                  ; definition of the constructor
                  (define ,(string->symbol (string-append "make-" s-s))
                    ; the constructor should be followed with field-value pairs
                    (lambda fvfv
                      (let (
                            ; make room for fields
                            (st (make-vector ,n+1))
                            ; WTF is the line below doing????
                            (ff ',ff))

                        ; the first element is a symbol - structure name
                        (vector-set! st 0 ',s)

                        ,@(let loop ((i 1) (r '()))
                            (if (> i n)
                              r
                              (loop (+ i 1)
                                    (cons `(vector-set! st ,i ,(vector-ref vv i)) r))))

                        (let loop ((fvfv fvfv))
                          (if (not (null? fvfv))
                            (begin
                              (vector-set! st 
                                           (+ (list-position (car fvfv) ff)
                                              1)
                                           (cadr fvfv))
                              (loop (cddr fvfv)))))
                        st)))

             ; defintion of field accessors
             ,@(let loop ((i 1) (procs '()))
                 (if (>= i n+1) procs
                     (loop (+ i 1)
                           (let ((f (symbol->string (list-ref ff (- i 1)))))

                             (cons
                              `(define ,(string->symbol (string-append s-s "." f))
                                 (lambda (x) (vector-ref x ,i)))
                              (cons
                               `(define ,(string->symbol (string-append "set!" s-s "." f))
                                  (lambda (x v) (vector-set! x ,i v)))
                               procs))))))

             ; definition of the judge function
             ; the symbol name is structure name + '?'
             (define ,(string->symbol (string-append s-s "?"))
               (lambda (x)
                 (and 
                   ; the instance should be a vector
                   (vector? x)
                   ; the first element should be the name of a structure
                   (eqv? (vector-ref x 0) ',s))))

                        )))))))
        (lambda (e r)
          (apply xfmr (cdr e))))))
