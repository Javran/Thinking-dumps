(load "../common/utils.scm")

; pass operations to the object
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle)
           (atan y x))
          (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(let ((x (make-from-real-imag 3 4)))
  (out (x 'real-part)
       (x 'imag-part)
       (x 'magnitude)
       (x 'angle)
       (apply-generic 'angle x)
       ))

(end-script)
