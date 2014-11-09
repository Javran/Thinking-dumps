(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./legacy-easy.scm")
(load "./legacy_lower_fast_patch.scm")

(load "./exercise_5_22_controllers.scm")

(load "./exercise_5_22_common.scm")

(define (my-append-machine x y)
  (let ((m (make-and-execute
            my-append-controller
            `((x ,x)
              (y ,y)))))
    (get-register-contents m 'result)))

(define (my-append!-machine x y)
  (let ((m (make-and-execute
            my-append!-controller
            `((x ,x)
              (y ,y)))))
    (get-register-contents m 'result)))

(test-my-append-machine
 my-append
 my-append-machine)

(test-my-append!-machine
 my-append!
 my-append!-machine)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
