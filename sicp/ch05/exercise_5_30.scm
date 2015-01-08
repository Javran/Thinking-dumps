(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu_ec_patch.scm")
(load "./exercise_5_23_common.scm")

(load "./exercise_5_30_error.scm")
(load "./exercise_5_30_machine.scm")

;; for simplicity, we just assume there is no syntax error.
;; I think this is a fair assumption since syntax error
;; can be detected even before running the program.

;; In addition, the errors suggested by the exercise are
;; also limited to runtime errors.

(ec-repl)

;; Local variables:
;; proc-entry: ""
;; End:
