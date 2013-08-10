(define (higher-type? a b)
  (define type-tower
    '(complex scheme-number rational))
  ; find index of a symbol
  (define (find-index x ls)
    (let ((tail (member x ls)))
      (if tail
        (- (length ls) (length tail))
        #f)))
  ; comparison
  (< (find-index a type-tower)
     (find-index b type-tower)))

; raise data to a higher type (curried)
(define (raise-to type)
  (lambda (data)
    (let ((data-type (type-tag data)))
      (cond ((equal? type data-type) data)
            ((higher-type? type data-type)
             ((raise-to type) (raise data)))
            (else (error "no way to raise downwards: RAISE-TO"))))))

; pick up the highest type in the list
(define (highest-type ls)
  (fold-left
    (lambda (cur-highest cur-type)
      (if (higher-type? cur-type cur-highest)
        cur-type
        cur-highest))
    (car ls)
    (cdr ls)))

; overwrite apply-generic
(define (apply-generic op . args)
  (define (raise-error)
    (error "No method for this argument list APPLY-GENERIC"
           (cons op args)))
  (define (same-type-list? ls)
    (apply boolean/and
           (map (lambda (x) (equal? x (car ls))) (cdr ls))))

  (let ((args-type (map type-tag args))
        (args-data (map contents args)))
    (let ((proc (get op args-type)))
      (if proc
        (apply proc args-data)
        ; if the handler is not found:
        (cond 
          ; if the argument list are already of the same type, abort.
          ((same-type-list? args-type) (raise-error))
          ; pick up the `highest` type, raise and retry
          (else
            (let* ((target-type (highest-type args-type))
                   (raised-args (map (raise-to target-type) args)))
              (apply apply-generic (cons op raised-args)))))))))

(define (install-coercion-test)
  (define (test)
    ; test higher-type?
    (let ((testcases
            (list (mat 'complex 'scheme-number #t)
                  (mat 'scheme-number 'complex #f)
                  (mat 'rational 'scheme-number #f)
                  (mat 'complex 'rational #t)
                  (mat 'scheme-number 'scheme-number #f))))
      (do-test-q higher-type? testcases))
    ; test highest-type
    (let ((testcases
            (list (mat 'scheme-number 'complex 'rational
                       'complex)
                  (mat 'rational 'rational 'scheme-number
                       'scheme-number)
                  (mat 'rational 'rational
                       'rational)
                  (mat 'scheme-number
                       'scheme-number)))
          (f (lambda args (highest-type args))))
      (do-test-q f testcases))
    ; test apply-generic
    (let* ((make-ra (get 'make 'rational))
           (make-sn (get 'make 'scheme-number))
           (make-ri (get 'make-ri 'complex))
           (gen-equ? (lambda (x y) (apply-generic 'equ? x y)))
           (a (make-ra 1 4))
           (b (make-sn 0.25))
           (c (make-ri 0.5 0)))
      (let ((testcases
              (list (mat 'add a b c)
                    (mat 'sub c a b)
                    (mat 'sub c b a))))
        (do-test-q apply-generic testcases gen-equ?)))

    )
  (put 'test 'coercion-system test)
  )
