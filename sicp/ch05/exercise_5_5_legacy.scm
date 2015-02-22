(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "exercise_5_5_controller.scm")

;; we only show if the simulator works,
;; the contents of the stack is not shown

(load "legacy-easy.scm")

(let ((m
       (make-and-execute
        fac-machine-controller
        `((n 5)))))
  (out (get-register-contents m 'val)))

(let ((m
       (make-and-execute
        fib-machine-controller
        `((n 5)))))
  (out (get-register-contents m 'val)))
