;; this is the simulator described in book
;; from now on I can this version "legacy"
(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_5_2_controller.scm")

(load "./legacy-simu.scm")

(define machine
  (make-machine
   '(product counter n result t1 t2)
   `( (+ ,+)
      (- ,-)
      (* ,*)
      (/ ,/)
      (zero? ,zero?)
      (> ,>)
      (>= ,>=)
      (< ,<)
      (<= ,<=)
      )
   (cdr factorial-machine)))

(set-register-contents! machine 'n 5)
(start machine)
(out (get-register-contents machine 'product))
;; 5 ! = 120
