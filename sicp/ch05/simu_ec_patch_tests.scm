(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu_ec_patch.scm")
(load "./ec-tests.scm")

(for-each (test-evaluator machine-eval) test-exps)

(end-script)
