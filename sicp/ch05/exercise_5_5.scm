(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "exercise_5_5_common.scm")

(load "exercise_5_5_controller.scm")

;; instead of hand-simulation, let's try to write one simulator

(ms-pretty-print
 (execute-controller-with-regs
  fac-machine-controller
  '((n 5))))

(ms-pretty-print
 (execute-controller-with-regs
  fib-machine-controller
  '((n 5))))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
