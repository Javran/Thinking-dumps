(load "../common/utils.scm")
(load "../common/test-utils.scm")

; to evaluate a combination:
; 1. evaluate the subexpression of the combination
;    how each argument is evaluated should always be considered
;    to be an implementation detail, don't reply on some particular order
; 2. apply the value of the operator subexpression to the value of
;    the operand subexpressions.

; syntactic sugar
(define (test-square sq)
  (out (map sq (list-in-range 1 10))))

(define (square1 x)
  (* x x))

(define square2
  (lambda (x) (* x x)))

; the result should be the same
(test-square square1)
(test-square square2)

; procedure:
; * procedure object: a pair
;   info about arguments + procedure body
; * environment part of a procedure
;   pointer to the environment in which the lambda expression
;   was evaluated to produce the procedure

; in general, `define` creates definitions by adding bindings to frames

; to apply a procedure to arguments:
; 1. create a new environment containing a frame that binds the parameters
;   to the values of the arguments.
; 2. the enclosing environment of this frame is the environment specified
;   by the procedure.
; 3. within this new environment, evaluate the procedure body

; summarized to 2 rules:
; * construct a frame, bind the formal parameters, evaluate the body
;   in the context of the new environment constructed
; * a procedure is created by evluating a lambda expression relative to
;   a given environment, the resulting procedure object is a pair:
;   * the text of the lambda expression
;   * a pointer to the environment in which the procedure was created

; `set!`:
; evaluating the expression `(set! <variable> <value>)` in some environment
; * locates the binding of the variable in the environment
; * changes that binding to indicate the new value

; here I show you an example that might disclose something about this model

(define (f) (out 'a))
(define (g) (f))
(g)
; a
(define (f) (out 'b))
(g)
; b

; the behavior of `g` changes when we redefine `f`.
; I guess that is exactly because the environment where
; `f` and `g` are defined is the same, redefining `f` results
; in a change of the current environment, and finally leads to
; the behavior change of `g`

(end-script)
