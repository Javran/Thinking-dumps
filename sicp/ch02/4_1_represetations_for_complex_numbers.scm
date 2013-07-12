(load "../common/utils.scm")

; complex number selectors:
; * real-part
; * imag-part
; * magnitude
; * angle

; complex number constructors:
; * make-from-real-imag
; * make-from-mag-ang

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-mag-ang (- (real-part z1) (real-part z2))
                     (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (test-complex)
  (let ((c1 (make-from-real-imag 1 1))
        (c2 (make-from-mag-ang 1 (/ pi 4))))
    (out (magnitude c1) ; ~= (sqrt 2)
         (angle c1) ; ~= pi/4
         (real-part c2) ; ~= (sqrt (/ 1 2))
         (imag-part c2) ; ~= (sqrt (/ 1 2))
         )))

(begin ; ben's impl
  (define real-part car)
  (define imag-part cdr)
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define make-from-real-imag cons)
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (test-complex))

(begin ; alyssa's impl
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define magnitude car)
  (define angle cdr)
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define make-from-mag-ang cons)
  (test-complex))

(end-script)
