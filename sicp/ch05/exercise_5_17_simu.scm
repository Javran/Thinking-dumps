(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./exercise_5_17_simu_prelabel_patch.scm")

(load "./figure_5_12.scm")

(let ((m (build-and-execute
          fib-machine-controller
          '((n 10)))))
  (out (machine-reg-get m 'val)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
