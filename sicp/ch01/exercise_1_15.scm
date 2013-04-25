(load "../common/utils.scm")

(define (sine ang)
  (define (cube x) (* x x x))
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (if (not (> (abs ang) 0.1))
    ; abs <= 0.1, x ~= (sine x)
    ang
    ; else
    (let ((ang/3 (/ ang 3.0)))
      (display "p is applied with ")
      (display ang/3)
      (newline)
      (p (sine ang/3)))))

(out (sin 10))
(out (sine 10))

; when the angle cannot be considered "small" enough (<= 0.1),
;     p will be applied with (/ angle 3.0)

; 12.15 => 4.05 => 1.35 => 0.45 => 0.15 => 0.05
; p should applied 5 times

; to verify, we can track the "if" statement...
(out (sine 12.15))
; "p is applied with ..." should be outputed for 5 times

; order of growth in space & number of steps:
; x(n) = x(n-1) / 3 => until x <= 0.1
; x <= 0.1 * 3^times, each time a constant space is taken(i.e. recursion state)
; => theta( log_3(a) )
