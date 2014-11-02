(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu_lower_patch.scm")

(let ((m (build-and-execute
          '(controller
            (assign a (const 10)))
          '())))
  (out (machine-reg-get m 'a)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
