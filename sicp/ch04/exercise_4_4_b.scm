(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define true? identity)
(define true-value #t)
(define false-value #f)

; (and) => #t
; (and <expr>) => <expr>
; (and <expr1> ...) =>
; ((lambda (result)
;    (if result
;       (and ...)
;       #f))
;  (eval <expr> env))
(define (eval-and exp env)
  (define (eval-and-aux exps env)
    (cond ((null?  'todo


(end-script)
