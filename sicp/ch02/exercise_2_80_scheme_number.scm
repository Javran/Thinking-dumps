(define (install-scheme-number-package)
  (define (=zero? x)
    (< (abs x) eps))

  (define (tag x)
    ; use symbol `scheme-number` as tag
    (attach-tag 'scheme-number x))

  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put '=zero? '(scheme-number) =zero?)

  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)
