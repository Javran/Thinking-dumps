(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_4_5_common.scm")

(out (cond->if '(cond ((proc1) conseq1)
                      ((proc2) conseq2)
                      (else alter))))

(end-script)
