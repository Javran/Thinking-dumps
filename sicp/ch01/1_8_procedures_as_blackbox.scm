(load "../common/utils.scm")

; each procedure accomplishes an identifiable task
;     that can be used as a module in defining other procedures.
; a procedure definition should be able to suppress detail.

; bound variable: the procedure definition binds its formal parameters(consistently renamed throughout the definition)
; free variable: not bounded

; The set of expressions for which a binding defines a name is called the scope of that name.

; "to localize the subprocedures"

; what we really care about is "my-sqrt", not "good-enough?" or "sqrt-iter"
; so we can wrap them inside
(define (my-sqrt x)
  ; the definition of "sqrt-iter", "good-enough?", "improve" and "average"
  ;     is limited inside this scope, i.e. only available inside the definition of "my-sqrt"
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (average x y)
    (/ (+ x y) 2))
  (sqrt-iter 1.0 x))

; here we have another way of limiting the definition of internal functions:
(define (my-sqrt-2 x)
  (letrec ((sqrt-iter 
           (lambda (guess x)
             (if (good-enough? guess x)
               guess
               (sqrt-iter (improve guess x) x))))
         (good-enough?
           (lambda (guess x)
             (< (abs (- (square guess) x)) 0.001)))
         (improve
           (lambda (guess x)
             (average guess (/ x guess))))
         (average 
           (lambda (x y)
             (/ (+ x y) 2))))
    (sqrt-iter 1.0 x)))

(out (my-sqrt 65536))
(out (my-sqrt-2 65536))
; ~ 256
