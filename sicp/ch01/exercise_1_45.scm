(load "../common/utils.scm")

; actually I think this solution is far from perfect
;     which sounds like just keep trying to use average-damping again and again
;     until it works ... too bad to call it a solution ...

(define (average-damping f)
  (lambda (x)
    (mid x (f x))))

(define tolerance 0.00001)

; we need a modified version that can keep a hint for how many times we've tried
; I'd like to try a limited time and if it doesn't work, we do average-damping on f
(define (fixed-point f first-guess allowed-try-time)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try f guess rest-try-time)
    (if (<= rest-try-time 0)
      ; try-time exhausted -> recharge and try again
      (begin
        (out "applying average-damping")
        (try (average-damping f) 1.0 allowed-try-time))
      (let ((next (f guess)))
        (if (close-enough? guess next)
          next
          (try f next (- rest-try-time 1))))))
  (try f first-guess allowed-try-time))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f time)
  (if (= time 0) 
    identity
    (lambda (x)
      (f ((repeated f (- time 1)) x)))))

(out (fixed-point
       (lambda (y) (/ 123456 (* y y y)))
       1.0
       16384))
; 2 average-damping is performed
; ~= 18.7447

; nth-root: find a value y that y^n = x 
; y^2 = x => y -> x/ y
; y^3 = x => y -> x/(y^2)
; y^n = x => y -> x/(y^(n-1))
(define (nth-root x n)
  (define (f y)
    (/ x (expt y (- n 1))))
  (fixed-point f 1.0 16384))

(out (nth-root (expt 123.4567 23) 23))
; 4 average-damping is performed

(for-each
  (lambda (testcase)
    (let* ((x (car testcase))
          (n (cdr testcase))
          (x^n (expt x n)))
      (display "Finding the nth-root of: ")
      (display x^n)
      (newline)

      (display "n = ")
      (display n)
      (newline)

      (display "the answer should be: ")
      (display x)
      (newline)

      (display "result: ")
      (newline)
      (display (nth-root x^n n))
      (newline)))
  '(
    (123.456 . 30)
    (567.8901 . 45)
    (23 . 78)))

(end-script)
