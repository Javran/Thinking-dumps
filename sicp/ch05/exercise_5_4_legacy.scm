(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_5_4_controller.scm")

(load "./legacy-easy.scm")

(out "expt-machine-1 (recursive):")
(let ((m
       (make-and-execute
        expt-machine-1
        `((b ,b) (n ,n)))))
  (out (get-register-contents m 'result)))

(out "expt-machine-2 (iterative):")
(let ((m
       (make-and-execute
        expt-machine-2
        `((b ,b) (n ,n)))))
  (out (get-register-contents m 'result)))
