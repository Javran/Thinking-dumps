(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "legacy-easy.scm")
(load "legacy_ec_patch.scm")
(load "ec-tests.scm")

(for-each (test-evaluator machine-eval) test-exps)
(newline)

(out
 (if (check-labels evaluator-insns)
     "no problem with the label checker"
     "some missing labels found"))

(end-script)
