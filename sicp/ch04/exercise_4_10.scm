(load "../common/utils.scm")
(load "../common/test-utils.scm")

; I want to summarize all the previous work done
; in this chapter, make `my-eval`, and expand it
; throughout this chapter.
(load "./my-eval.scm")

; let's define an `unless` expression,
; (unless <pred> <exp1> <exp2>)
; => (if <pred> <exp2> <exp1>)

(define (unless->if exp)
  (let ((unless-pred (cadr exp))
        (unless-exp1 (caddr exp))
        (unless-exp2 (cadddr exp)))
    (make-if unless-pred unless-exp2 unless-exp1)))

(define (eval-unless exp env)
  (my-eval (unless->if exp) env))
    
(define (test-unless)
  (let ((env (init-env)))
    (do-test
      eval-unless
      (list
        (mat `(unless (< 1 10) 10 20) env 20)
        (mat `(unless (= 1 10) 10 20) env 10)
        (mat `(unless (> 1 10) 10 20) env 10)
        ))
    'ok))

(define handler
  (make-handler
    'unless
    eval-unless
    test-unless))

(handler-register! handler)

(my-eval-test-installed-handlers)

(end-script)
