(define (install-scheme-number-package)

  (define (=zero? x) (< (abs x) eps))
  (define (equ? x y) (=zero? (- x y)))

  (define (test-package)
    (let* ((make (get 'make 'scheme-number))
           (x (make 1))
           (y (make 2))
           (z (make 1.0))
           (equ-num? (lambda (a b) (apply-generic 'equ? a b))))
      (let ((testcases
              (list (mat x y #f)
                    (mat y z #f)
                    (mat x z #t))))
        (do-test-q equ-num? testcases))
      (let ((testcases
              (list
                (mat 'add x y (make 3))
                (mat 'sub y x z)
                (mat 'mul y y (make 4))
                (mat 'div y y (make 1))))
            (f (lambda (tag a1 a2)
                 (apply-generic tag a1 a2))))
        (do-test-q f testcases equ-num?))
      (let ((testcases
              (list
                (mat 'to-string x "1")
                (mat 'to-string y "2"))))
        (do-test-q apply-generic testcases))
      ))

  (put 'make 'scheme-number (tagged 'scheme-number identity))
  (put 'add '(scheme-number scheme-number) (tagged 'scheme-number +))
  (put 'sub '(scheme-number scheme-number) (tagged 'scheme-number -))
  (put 'mul '(scheme-number scheme-number) (tagged 'scheme-number *))
  (put 'div '(scheme-number scheme-number) (tagged 'scheme-number /))
  (put 'equ? '(scheme-number scheme-number) equ?)
  (put '=zero? '(scheme-number) =zero?)
  (put 'to-string '(scheme-number) number->string)
  (put 'test 'scheme-number-package test-package)

  'done)
