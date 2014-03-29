(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (square x)
  (* x x))
; global environment += definition of `square`

(define (sum-of-squares x y)
  (+ (square x) (square y)))
; global environment += definition of `sum-of-squares`

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
; global environment += definition of `f`

(out (f 5))
; notation: <expr> : <envlist>
;   where <expr> is an expression
;         <envlist> is a list of environment
;         if <envlist> = A::B, A is inside any element of B
; let global environment = G
; (f 5): [G]
; (sum-of-squares (+ a 1) (* a 2)): E1:[G],
;   where a = 5 in E1
; (sum-of-squares 6 10) : E1:[G]
;   (+ (square x) (square y)) : E2:[E1,G]
;     where x = 6, y = 10 in E2
;     (square x) : E3:[E2,E1,G]
;       where x = 6 in E3
;     (square x) : E4:[E2,E1,G]
;       where x = 10 in E4
;   (+ 36 100) : E2:[E1,G]
; 136 

(end-script)
