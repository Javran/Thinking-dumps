(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_4_5_common.scm")

(define def-env user-initial-environment)

(define (eval-cond exp env)
  (eval (cond->if exp) env))

(define (clause-arrow? clause)
  (eq? (cadr clause) '=>))
; (<predicate> => <handler>)
;                    ^- the third element
(define clause-handler caddr)

(define (expand-clauses clauses)
  (if (null? clauses)
    ; no case is given
    'false
    (let ((first (car clauses))
          (rest  (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          ; else part ... convert the seq to exp
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last: COND->IF"
                 clauses))
        ; ((lambda (result)
        ;    (if result
        ;      <action>
        ;      ...))
        ;  <predicate>)
        (let ((result-sym (gensym)))
          ; make an application
          (list
            ; operator
            (make-lambda
              ; parameters
              (list result-sym)
              ; body
              (list
                (make-if
                  result-sym ; use cached result
                  (if (clause-arrow? first)
                    ; the extended syntax
                    ;   should be an application
                    (list
                      ; operator
                      (clause-handler first)
                      ; operand
                      result-sym)
                    ; the original syntax
                    (sequence->exp (cond-actions first)))
                  (expand-clauses rest))))
            ; operand
            (cond-predicate first)))))))

(out (eval-cond 
       '(cond ((assoc (quote b)
                      (quote ((a 1) (b 2))))
               =>
               cadr)
              (else false))
       def-env))

(end-script)
