(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu_listprim_patch.scm")

(load "./exercise_5_22_controllers.scm")

(let ((m (build-and-execute
          my-append-controller
          '((x (1 2 3))
            (y (4 5 6))))))
  (out (machine-reg-get m 'result)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
