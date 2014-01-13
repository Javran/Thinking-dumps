(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_4_4_common.scm")

(define (eval-and exp env)
  ; evaluate expressions, short circuit if necessary
  (define (eval-and-aux exps env)
    (cond ((null? exps) true-value)
          ((null? (cdr exps))
            ; we are dealing with the last element in the list
            ; simply evaluating it will do
            (eval (car exps) env))
          (else
            (if (true? (eval (car exps) env))
              ; need further examination
              (eval-and-aux (cdr exps) env)
              ; else short circuit
              #f))))
  (eval-and-aux (cdr exp) env))

(define (eval-or exp env)
  ; evaluate expressions
  (define (eval-or-aux exps env)
    (cond ((null? exps) false-value)
          ((null? (cdr exps))
            (eval (car exps) env))
          (else
            (let ((result (eval (car exps) env)))
              (if (true? result)
                result
                (eval-or-aux (cdr exps) env))))))
  (eval-or-aux (cdr exp) env))

(test-and-or eval-and eval-or)

(end-script)
