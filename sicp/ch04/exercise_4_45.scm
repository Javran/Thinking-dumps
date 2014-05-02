(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")
(load "./natural_language_common.scm")

(for-each
 (lambda (e) (pretty-print e) (newline))
 (amb-eval-all
  (run-source-in-env
   `(parse '(the professor lectures to the student in the class with the cat)))
  (amb-init-env)))

;; rearrange to help giving interpretations for each sentences:
;; * the professor (((lectures to the student) in the class) with the cat)
;; * the professor ((lectures to the student) (in (the class with the cat)))
;; * the professor ((lectures to (the student in the class)) with the cat)
;; * the professor (lectures to (the student in the class with the cat))
;; * the professor lectures to (the student in the class with the cat)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
