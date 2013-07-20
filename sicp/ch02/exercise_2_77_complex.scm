(load "./4_3_data_directed_aly_impl.scm")
(load "./4_3_data_directed_ben_impl.scm")

(install-rect-package)
(install-polar-package)

(define (install-complex-package)
  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (angle z)
    (apply-generic 'angle z))
  (define make-from-real-imag
    (get 'make-from-real-imag 'rect))
  (define make-from-mag-ang
    (get 'make-from-mag-ang 'polar))

  (define (tag z) (attach-tag 'complex z))
  (define (tagged f) (lambda args (tag (apply f args))))
  (put 'make-from-real-imag 'complex (tagged make-from-real-imag))
  (put 'make-from-mag-ang 'complex (tagged make-from-mag-ang))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  'done)
