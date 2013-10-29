(load "../common/utils.scm")
(load "../common/test-utils.scm")

; this is the global environment
(define (my-sqrt x)
  ; this is the environment E1
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(out (my-sqrt 100))
; ~ 10

; two key points:
; * the name of the local procedure do not interfere with
;   names external to the enclosing procedure
; * the local procedure can access the arguments of
;   the enclosing procedure simply by using parameter names
;   as free variables

(end-script)
