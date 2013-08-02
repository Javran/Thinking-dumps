(define (install-polar-package)

  (define make-real (get 'make 'real))
  (define (make-num x) (drop (make-real x)))

  ;; internal procedures
  (define (real-part z)
    (make-num (* (num->value (magnitude z)) ((unwrapped cos) (angle z)))))
  (define (imag-part z)
    (make-num (* (num->value (magnitude z)) ((unwrapped sin) (angle z)))))

  (define magnitude car)
  (define angle cdr)

  (define make-from-mag-ang cons)
  (define (make-from-real-imag x y)
    (make-from-mag-ang (sqrt (+ (square x) (square y)))
                       (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
