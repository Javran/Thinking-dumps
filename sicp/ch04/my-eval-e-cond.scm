;; handle cond-exp
;; from ./exercise_4_5.scm

;; (cond <clause #1>
;;       <clause #2>
;;       ...
;;       [(else ...)])

;; a list of clauses
(define cond-clauses cdr)

;; each clause:
;; (<exp1> <seq-of-exps>)
;; <exp1>: predicate
;; <seq-of-exps>: actions
(define cond-predicate car)
(define cond-actions cdr)


(define (clause-arrow? clause)
  (eq? (cadr clause) '=>))

;; (<predicate> => <handler>)
;;                    ^- the third element
(define clause-handler caddr)

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond->if exp)
  (define (expand-clauses clauses)
    (if (null? clauses)
        ;; no case is given
        'false
        (let ((first (car clauses))
              (rest  (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  ;; else part ... convert the seq to exp
                  (sequence->exp (cond-actions first))
                  (error "ELSE clause isn't last: COND->IF"
                         clauses))
              ;; ((lambda (result)
              ;;    (if result
              ;;      <action>
              ;;      ...))
              ;;  <predicate>)
              (let ((result-sym (gensym)))
                ;; make an application
                (list
                 ;; operator
                 (make-lambda
                  ;; parameters
                  (list result-sym)
                  ;; body
                  (list
                   (make-if
                    result-sym          ; use cached result
                    (if (clause-arrow? first)
                        ;; the extended syntax
                        ;;   should be an application
                        (list
                         ;; operator
                         (clause-handler first)
                         ;; operand
                         result-sym)
                        ;; the original syntax
                        (sequence->exp (cond-actions first)))
                    (expand-clauses rest))))
                 ;; operand
                 (cond-predicate first)))))))
  (expand-clauses (cond-clauses exp)))

(define (install-eval-cond)

  (define (eval-cond exp env)
    (my-eval (cond->if exp) env))

  (define (analyze-cond exp)
    (my-analyze (cond->if exp)))

  (define (test-eval eval-cond)
    (define env
      (init-env))

    (define cond-test-exp-1
      `(cond ((= a 0) 2)
             ((= a 1) 1)
             ((= a 2) 0)))

    (define cond-test-exp-2
      `(cond ((= a 0) 10)
             (else 10 15 20)))

    (define cond-test-exp-3
      ; test arrow extension
      `(cond ((= a 0) => (lambda (x) (if x 10 20)))
             ((= a 1) => (lambda (x) (if x 30 40)))
             ((= a 2) 50)
             (else 60)))

    (do-test
      eval-cond
      (list
        (mat cond-test-exp-1
             (extend-environment
               '(a) '(0) env) 2)
        (mat cond-test-exp-1
             (extend-environment
               '(a) '(1) env) 1)
        (mat cond-test-exp-1
             (extend-environment
               '(a) '(2) env) 0)
        (mat cond-test-exp-2
             (extend-environment
               '(a) '(0) env) 10)
        (mat cond-test-exp-2
             (extend-environment
               '(a) '(1) env) 20)
        ; test arrow extension
        (mat cond-test-exp-3
             (extend-environment
               '(a) '(0) env) 10)
        (mat cond-test-exp-3
             (extend-environment
               '(a) '(1) env) 30)
        (mat cond-test-exp-3
             (extend-environment
               '(a) '(2) env) 50)
        (mat cond-test-exp-3
             (extend-environment
               '(a) '(3) env) 60)
        ))
    'ok)

  (define handler
    (make-handler
      'cond
      eval-cond
      analyze-cond
      (test-both
       test-eval
       eval-cond
       analyze-cond)))

  (handler-register! handler)
  'ok)
;; Local variables:
;; proc-entry: "./my-eval.scm"
;; End:

