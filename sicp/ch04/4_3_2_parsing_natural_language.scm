(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")

(load "./natural_language_common.scm")

;; should be only single one result:
(for-each
 out
 (amb-eval-all
  (run-source-in-env
   `(parse '(the student with the cat sleeps in the class)))
  (amb-init-env)))
(newline)

;; test for nondeterminism ..
;; this one should have multiple results
(for-each
 out
 (amb-eval-all
  (run-source-in-env
   `(parse '(the professor lectures to the student with the cat)))
  (amb-init-env)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
