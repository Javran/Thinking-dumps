(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define def-env user-initial-environment)

(define true? identity)
(define true-value #t)
(define false-value #f)

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

(let ((testcases
        (list
          (mat '(and) #t)
          (mat '(and (= 1 1) (= 2 2)) #t)
          (mat '(and (= 1 1) #f (error 'wont-reach)) #f)))
      (proc
        (lambda (exp)
          (eval-and exp def-env))))
  (do-test proc testcases))

(let ((testcases
        (list
          (mat '(or) #f)
          (mat '(or #t (error 'wont-reach)) #t)
          (mat '(or (< 1 1) (> 2 2)) #f)))
      (proc
        (lambda (exp)
          (eval-or exp def-env))))
  (do-test proc testcases))

(end-script)
