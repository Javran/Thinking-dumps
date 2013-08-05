(define (install-rect-package)

  (define make cons)
  (define real-part car)
  (define imag-part cdr)

  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))

  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (test-package)
    (let* ((make (get 'make 'rect))
            (x1 (make 1 2))
            (x2 (make 3 4)))
       (let ((testcases
               (list (mat 'real-part x1 1)
                     (mat 'imag-part x1 2)
                     (mat 'real-part x2 3)
                     (mat 'imag-part x2 4)
                     (mat 'magnitude x1 (sqrt 5))
                     (mat 'angle x2 (atan 4 3)))))
         (do-test-q apply-generic testcases (close-number? eps)))
       ))

  (put 'real-part '(rect) real-part)
  (put 'imag-part '(rect) imag-part)
  (put 'magnitude '(rect) magnitude)
  (put 'angle '(rect) angle)
  (put 'make 'rect (tagged 'rect make))
  (put 'test 'rect-package test-package)
  'done)
