(load "../common/utils.scm")
(load "../common/test-utils.scm")

; to summarize, objects can have same code
;   but run in different environment
;   which lead to the different behavior.

(define (object a)
  (define (run)
    (set! a (inc a))
    (out a))
  run)

(define x (object 0))
(define y (object 0))

(x)(x)(x)
; 1 2 3
(y)(y)(y)
; 1 2 3
; by `different` I meant `x` and `y` do not share
;   their environments.

(end-script)
