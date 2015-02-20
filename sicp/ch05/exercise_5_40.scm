(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; ctenv: compile-time environment

;; frames are organized as a list
;; in a stack-style, the newer one
;; will be attached to the head of the list
(define (empty-ctenv)
  '())

(define empty-ctenv? null?)

(define first-ctframe car)
(define enclosing-ctenv cdr)

(define (extend-ctenv vars ctenv)
  (cons (make-ctframe vars) ctenv))

;; frames only need to contain variable names
;; since we neither know nor need the values
;; at compile time

;; keep the variable name list as it is
(define make-ctframe identity)

;; (symbol, ctframe) -> ctframe
(define add-binding-to-ctframe cons)

;; TODO: for now I have no idea how to deal with "define" bindings.
;; we have to come back later.

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
