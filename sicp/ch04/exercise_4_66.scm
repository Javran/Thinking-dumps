(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./4_4_1_rules.scm")

;; I've implemented a new handler for qeval:
;; lisp-eval handler:
;; (lisp-eval function return-pat arg1 arg2 ...)
;; this handler will try to evaluate (function arg1 arg2 ...)
;; and its result will be pattern matched against return-pat
;; this function essentially lifts lisp functions into the query system.
(apply
 qe-asserts!
 '(
   (num 1) (num 2) (num 3)
   (rule (prod ?x ?y ?z)
         (and (num ?x)
              (num ?y)
              (lisp-eval * ?z ?x ?y)))))

(out (qe-all '(prod ?x ?y ?z)))

;; previously I thought the "lisp-eval" handler will help implementing
;; accumulation functions, but it turns out not being helpful as expected.
;; the problem is that while "lisp-eval" deals with one result at a time
;; accumulation functions tend to collect all results and perform operations
;; on them.

;; Since the problem only needs us to outline a method to salvage the situation
;; I recommend adding a handler to filter out duplicate elemeents.
;; and accumulation functions should be used on the filtered list.

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
