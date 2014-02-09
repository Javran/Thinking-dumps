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

(end-script)
