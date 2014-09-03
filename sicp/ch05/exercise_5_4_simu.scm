(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_5_4_controller.scm")

(load "./simu.scm")

(out "expt-machine-1 (recursive):")
(let ((m
       (build-and-execute
        expt-machine-1
        `((b ,b) (n ,n)))))
  (out (machine-reg-get m 'result)))

(out "expt-machine-2 (iterative):")
(let ((m
       (build-and-execute
        expt-machine-2
        `((b ,b) (n ,n)))))
  (out (machine-reg-get m 'result)))

