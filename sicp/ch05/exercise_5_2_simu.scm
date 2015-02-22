;; this is the "simu" version of the simulator
;; in action
(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "exercise_5_2_controller.scm")

(load "simu.scm")

(let ((m (build-and-execute
          factorial-machine
          '((n 5)))))
  (out (machine-reg-get m 'product)))
