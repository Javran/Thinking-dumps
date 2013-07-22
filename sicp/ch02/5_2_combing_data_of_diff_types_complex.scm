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

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))

  (define (tag z) (attach-tag 'complex z))
  (define (tagged f) (lambda args (tag (apply f args))))

  (define (add-complex-to-schemenum z x)
    (make-from-real-imag (+ (real-part z) x)
                         (imag-part z)))

  (put 'add '(complex complex) (tagged add-complex))
  (put 'add '(complex scheme-number) (tagged add-complex-to-schemenum))

  (put 'make-from-real-imag 'complex (tagged make-from-real-imag))
  (put 'make-from-mag-ang 'complex (tagged make-from-mag-ang))
  'done)
