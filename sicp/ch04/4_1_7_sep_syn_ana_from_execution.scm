(load "../common/utils.scm")
(load "../common/test-utils.scm")

; the idea:
;   the inefficiency is caused by
;   interleaving the evaluation with
;   the syntactic analysis
;   while the analysis can actually be done
;   individually.
;
; solution:
;   try to do the syntactic analysis
;   only once.
;   let another procedure to do the analysis
;   and return "the execution procedure"

; the framework will be like:
;   (define (eval exp env)
;     ((analyze exp) env))

(define (analyze-self-evaluating exp)
  ; for self-evaluating expressions,
  ;   we wrap the result inside a function
  ;   and when this function be called with
  ;   an environment, this function returns `exp`
  (lambda (env) exp))

(define (analyze-quoted exp)
  ; fetch text of quotation, same way as before
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  ; first one that uses the given env
  (lambda (env)
    (lookup-variable-value exp env)))


(end-script)
