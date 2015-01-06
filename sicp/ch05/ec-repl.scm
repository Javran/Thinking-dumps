(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu_ec_patch.scm")

(out (map car (default-ops-builder (empty-machine))))

(load "./exercise_5_23_common.scm")
(load "./ec-eval_v2.scm")

(out (map car (default-ops-builder (empty-machine))))

(ec-repl)

;; Local variables:
;; proc-entry: ""
;; End:
