(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "simu_lower_fast_patch.scm")

(load "exercise_5_21_controllers.scm")
(load "exercise_5_21_common.scm")

(define (count-leaves-r-machine tree)
  ;; evaluate the function by running the machine
  (let ((m (build-and-execute
            count-leaves-r-controller
            `((tree ,tree)))))
    (machine-reg-get m 'result)))

(define (count-leaves-i-machine tree)
  ;; evaluate the function by running the machine
  (let ((m (build-and-execute
            count-leaves-i-controller
            `((tree ,tree)))))
    (machine-reg-get m 'result)))

;; two different approaches should have the same result
(test-machine
 count-leaves-r
 count-leaves-i)

;; scheme impl vs. machine (recursive version)
(test-machine
 count-leaves-r
 count-leaves-r-machine)

;; scheme impl vs. machine (iterative version)
(test-machine
 count-leaves-i
 count-leaves-i-machine)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
