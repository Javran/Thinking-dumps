(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./exercise_5_16_simu_tracing_patch.scm")

(load "./figure_5_12.scm")

;; TODO: make trace controllable through
;; instructions

;; TODO: write a small controller text
;; to test turn on / off functionality

(let ((m (build-with fib-machine-controller
                     '((n 5))
                     default-ops-builder)))
  (machine-trace-on! m)
  (machine-fresh-start! m))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
