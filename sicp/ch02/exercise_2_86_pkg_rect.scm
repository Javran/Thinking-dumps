(define (install-rect-package)
  ;; internal procedures
  (define real-part car)
  (define imag-part cdr)

  (define make-real (get 'make 'real))
  (define (make-num x) (drop (make-real x)))

  (define (magnitude z)
    (make-num
      (sqrt (+ ((unwrapped square) (real-part z))
               ((unwrapped square) (imag-part z))))))

  (define (angle z)
    (make-num
      ((unwrapped atan) (imag-part z) (real-part z))))

  (define make-from-real-imag cons)

  ;; interface to the rest of the system
  ; the "'(rect)" shown above is actually an equivalent to type signature
  ; when replaced with "'(rect polar)", it might suggest the proc in the value field
  ;   is a binary that accept 2 arguments with type 'rect and 'polar respectively
  (define (tag x) (attach-tag 'rect x))
  (put 'real-part '(rect) real-part)
  (put 'imag-part '(rect) imag-part)
  (put 'magnitude '(rect) magnitude)
  (put 'angle '(rect) angle)
  (put 'make-from-real-imag 'rect 
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)
