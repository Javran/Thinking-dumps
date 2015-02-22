;; this is the simulator described in book
;; from now on I can this version "legacy"
(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "exercise_5_2_controller.scm")

(load "legacy-easy.scm")

(define machine
  (make-and-execute
   factorial-machine
   '((n 5))))
(out (get-register-contents machine 'product))
