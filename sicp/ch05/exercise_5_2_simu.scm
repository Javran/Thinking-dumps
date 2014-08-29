(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_5_2_controller.scm")

(load "./simu.scm")

(let ((m (build-and-execute (cdr factorial-machine))))
  (machine-reg-get m 'result))
