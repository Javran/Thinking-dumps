(define (higher-type? a b)
  (define type-tower
    '(polynominal complex scheme-number rational))
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
          ((null? args-type) (raise-error))
          ; if the argument list are already of the same type, try raise all up then abort.
          ((same-type-list? args-type)
            (let ((raise-up (get 'raise (list (car args-type)))))
              (if raise-up
                (apply apply-generic (cons op (map raise-up args-data)))
                (raise-error))))
          ; pick up the `highest` type, raise and retry
          (else
            (let* ((target-type (highest-type args-type))
                   (raised-args (map (raise-to target-type) args)))
              (apply apply-generic (cons op raised-args)))))))))

(define (drop datum)
  (define (do-drop datum type)
    (let ((proc (get 'project (list type))))
      (if proc
        (let ((projected (project datum)))
          (if (equ? (raise projected) datum)
            (do-drop projected (type-tag projected))
            datum))
        datum)))
  (let ((type (type-tag datum)))
    ; valid type?
    (if type
      (do-drop datum type)
      datum)))

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
    ; test drop
    (let* ((make-ri (get 'make-ri 'complex))
           (make-ma (get 'make-ma 'complex))
           (make-ra (get 'make 'rational))
           (make-sc (get 'make 'scheme-number))
           (a (make-ri 1 2))
           (b (make-ma 5 0))
           (c (make-ra 5 1))
           (d (make-sc 5)))
      (let ((testcases
              (list (mat a 'complex)
                    (mat b 'rational)
                    (mat c 'rational)
                    (mat d 'rational)))
            (f (lambda (x) (type-tag (drop x)))))
        (do-test-q f testcases)))
    )
  (put 'test 'coercion-system test)
  )
