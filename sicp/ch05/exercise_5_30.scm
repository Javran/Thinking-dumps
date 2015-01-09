(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu_ec_patch.scm")
(load "./exercise_5_23_common.scm")

(load "./exercise_5_30_common.scm")
(load "./exercise_5_30_machine.scm")

;; for simplicity, we just assume there is no syntax error.
;; I think this is a fair assumption since syntax error
;; can be detected even before running the program.

;; In addition, the errors suggested by the exercise are
;; also limited to runtime errors.

;; Actually "a." and "b." are corresponding to
;; machine primitive errors and implemented language primitive errors
;; respectively.
;; for machine primitive errors one can use "make-error"
;; for implemented language errors some beforehand checks can be done
;; (or we can even "cheat" by letting "apply-primitive-procedure" capture
;; errors and wrap it properly)

;; Also I won't deal with errors like "accessor applied on a wrong value",
;; this is just boring and makes the code verbose.

;; TODO: some tests?
(load "./exercise_5_30_cheat.scm")

(ec-repl)

;; TODO: do part b in a "cheating" way:
;; * apply-primitive-procedure

;; Local variables:
;; proc-entry: ""
;; End:
