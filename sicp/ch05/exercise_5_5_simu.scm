(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_5_5_controller.scm")

;; we only show if the simulator works,
;; the contents of the stack is not shown

(load "./simu.scm")

(let ((m
       (build-and-execute
        fac-machine-controller
        `((n 5)))))
  (out (machine-reg-get m 'val)))

(let ((m
       (build-and-execute
        fib-machine-controller
        `((n 5)))))
  (out (machine-reg-get m 'val)))
