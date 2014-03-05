(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./my-eval.scm")
(load "./exercise_4_31_promise.scm")

(load "./exercise_4_31_delay_force.scm")

(test-promise)

(install-eval-delay)
(install-eval-force)

(newline)
(out "===== new extensions are inserted, retesting ...")

(my-eval-test-all)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
