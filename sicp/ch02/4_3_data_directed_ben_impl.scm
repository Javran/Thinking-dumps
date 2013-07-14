(define (install-rect-package)
  ;; internal procedures
  (define real-part car)
  (define imag-part cdr)

  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define make-from-real-imag cons)
  (define (make-from-mag-ang r a)
    (make-from-real-imag (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rect x))
  (put 'real-part '(rect) real-part)
  (put 'imag-part '(rect) imag-part)
  (put 'magnitude '(rect) magnitude)
  (put 'angle '(rect) angle)
  (put 'make-from-real-imag 'rect 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rect
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
