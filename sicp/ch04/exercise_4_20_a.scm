(load "../common/utils.scm")
(load "../common/test-utils.scm")

; (letrec ((<var1> <exp1>)
;          (<var2> <exp2>)
;          (<var3> <exp3>)
;          ...)
;   <body>)
; the environment setup:
; for <exp1>, <exp2>, ...,
;   the variables <var1>, <var2> ... are shared among each other.
;   Since we are just defining variables, we don't need to care
;   about the availablility of the value a variable is bound to.
;   simply assign them anything will do.
; I don't want to handle the special case `*unassigned*` for now,
;   because I'm thinking about invention something else that can never
;   been created from the implemented language.
; let's transform it into:
; (let ((<var1> '*unassigned*)
;       (<var2> '*unassigned*)
;       ...)
;   (set! <var1> <exp1>)
;   (set! <var2> <exp2>)
;   ...
;   <body>)
; but this is the scan-out for `let` from 4.1.6 ... correct?

(define (letrec->let exp)
  (define binding-pairs
    (cadr exp))
  (define vars (map car  binding-pairs))
  (define exps (map cadr binding-pairs))
  (define body
    (cddr exp))
  `(let ,(map (lambda (var)
                   `(,var '*unassigned*))
                 vars)
     ,@(map (lambda (var exp) `(set! ,var ,exp))
            vars exps)
     ,@body))

(do-test
  (lambda (exp1)
    (eval (letrec->let exp1)
          user-initial-environment))
  (list
    (mat `(letrec ((fact (lambda (n)
                           (if (= n 0)
                             1
                             (* n (fact (- n 1)))))))
            (fact 4))
         24)
    (mat `(letrec ((f1 (lambda (n)
                         (if (= n 0)
                           1
                           (* n (f2 (- n 1))))))
                   (f2 (lambda (n)
                         (if (= n 0)
                           1
                           (* n (f3 (- n 1))))))
                   (f3 (lambda (n)
                         (if (= n 0)
                           1
                           (* n (f1 (- n 1)))))))
            (f3 10))
         3628800)
    ))

(end-script)
