(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "exercise_5_6_controller.scm")

(load "legacy-easy.scm")

(let ((m
       (make-and-execute
        fib-machine-controller-mod
        `((n 5)))))
  (out (get-register-contents m 'val)))
