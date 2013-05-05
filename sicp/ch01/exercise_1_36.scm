(load "../common/utils.scm")

(define tolerance 0.00001)

; track guessing time
(define (verbose-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess try-time)
    (display "guess: ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        (cons next try-time)
        (try next (inc try-time)))))
  (try first-guess 0))

; try to find the fixed point of cosine
(out (verbose-fixed-point cos 1.0))

(out "without average damping:")
(out (verbose-fixed-point
       (lambda (x)
         (/ (log 1000) (log x)))
       100.0))
; 35 times

(out "with average damping:")
(out (verbose-fixed-point
       (lambda (x)
         (mid x
              (/ (log 1000) (log x))))
       100.0))
; 12 times
