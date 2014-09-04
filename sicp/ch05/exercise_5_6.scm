(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_5_5_common.scm")
(load "./exercise_5_6_controller.scm")

(ms-pretty-print
 (execute-controller-with-regs
  fib-machine-controller-mod
  '((n 5))))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
