(define (install-poly-term-package)

  (define make cons)
  (define order car)
  (define coeff cdr)

  (define zero-term? (compose =zero? coeff))
  (define (term-equ? a b)
    (and (=    (order a) (order b))
         (equ? (coeff a) (coeff b))))

  (define (add-term t1 t2)
    (assert (= (order t1) (order t2))
            "term order must be same")
    (make (order t1)
          (add (coeff t1) (coeff t2))))

  (define (mul-term t1 t2)
    (make (+   (order t1) (order t2))
          (mul (coeff t1) (coeff t2))))

  (define (test)
    (let ((make (get 'make 'poly-term)))
      (let ((x1 (make 1 (make-rational 2 3)))
            (x2 (make 5 (make-scheme-number 5)))
            (x3 (make 5 (make-rational 25 5)))
            (x4 (make 1 (make-rational 25 5))))
        ; test accessors
        (let ((testcases 
                (list (mat 'order x1 1)
                      (mat 'order x2 5))))
          (do-test-q apply-generic testcases))
        (let ((testcases
                (list (mat 'coeff x1 (make-scheme-number (/ 2 3)))
                      (mat 'coeff x2 (make-complex-ri 5 0)))))
          (do-test-q apply-generic testcases equ?))
        ; test =zero?
        (let ((testcases
                (list (mat '=zero? x1 #f)
                      (mat '=zero? x2 #f)
                      (mat '=zero? (make 100 (make-scheme-number 0)) #t))))
          (do-test-q apply-generic testcases))
        ; test equ?
        (let ((testcases
                (list (mat 'equ? x1 x2 #f)
                      (mat 'equ? x1 x3 #f)
                      (mat 'equ? x2 x3 #t)
                      (mat 'equ? x3 x4 #f))))
          (do-test-q apply-generic testcases))
        ; test add
        (let ((testcases
                (list (mat 'add x1 x4 (make 1 (make-scheme-number (+ 5 2/3))))
                      (mat 'add x2 x3 (make 5 (make-scheme-number 10))))))
          (do-test-q apply-generic testcases equ?))
        )))
  (put 'make 'poly-term (tagged 'poly-term make))
  (put 'order '(poly-term) order)
  (put 'coeff '(poly-term) coeff)
  (put '=zero? '(poly-term) zero-term?)
  (put 'equ? '(poly-term poly-term) term-equ?)
  (put 'add '(poly-term poly-term) (tagged 'poly-term add-term))

  (put 'test 'poly-term-package test)

  'done)
