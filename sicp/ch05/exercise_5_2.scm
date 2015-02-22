(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "exercise_5_2_controller.scm")

(load "exercise_5_5_common.scm")

(ms-pretty-print
 (execute-controller-with-regs
  factorial-machine
  '((n 5))))
