(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./exercise_5_17_simu_prelabel_patch.scm")

(load "./figure_5_12.scm")

;; TODO: show that it does not interfere with instruction counting

(let ((m (build-with
          fib-machine-controller
          '((n 10))
          default-ops-builder)))
  (machine-trace-on! m)
  (machine-fresh-start! m)
  (out (machine-reg-get m 'val)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
