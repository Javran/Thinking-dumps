(define (install-scheme-number-package)
  (define (tag x)
    ; use symbol `scheme-number` as tag
    (attach-tag 'scheme-number x))

  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))

  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))

  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)
