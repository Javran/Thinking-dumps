(define (install-poly-term-package)

  (define (make o c) (list o c))
  (define order car)
  (define coeff cadr)

  (define (test)
    (let ((make (get 'make 'poly-term)))
      (let ((x1 (make 1 (make-rational 2 3)))
            (x2 (make 5 (make-scheme-number 5))))
        (let ((testcases 
                (list (mat 'order x1 1)
                      (mat 'order x2 5))))
          (do-test-q apply-generic testcases))
        (let ((testcases
                (list (mat 'coeff x1 (make-scheme-number (/ 2 3)))
                      (mat 'coeff x2 (make-complex-ri 5 0)))))
          (do-test-q apply-generic testcases equ?))
        )
      ))

  (put 'make 'poly-term (tagged 'poly-term make))
  (put 'order '(poly-term) order)
  (put 'coeff '(poly-term) coeff)

  (put 'test 'poly-term-package test)

  'done)
