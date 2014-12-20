(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu_ec_patch.scm")

(load "./exercise_5_23_common.scm")
(load "./exercise_5_24_common.scm")
(load "./exercise_5_24_machine.scm")

(for-each (test-evaluator machine-eval) test-exps)
(newline)
;; TODO some tests

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
