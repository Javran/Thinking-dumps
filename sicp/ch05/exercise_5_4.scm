(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_5_5_common.scm")

(load "./exercise_5_4_controller.scm")

(out "expt-machine-1 (recursive):")
(ms-pretty-print
 (execute-controller-with-regs
  expt-machine-1
  `((b ,b) (n ,n))))

(out "expt-machine-2 (iterative):")
(ms-pretty-print
 (execute-controller-with-regs
  expt-machine-2
  `((b ,b) (n ,n))))
