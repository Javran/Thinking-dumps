(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "exercise_5_6_controller.scm")

(load "simu.scm")

(let ((m
       (build-and-execute
        fib-machine-controller-mod
        `((n 5)))))
  (out (machine-reg-get m 'val)))
