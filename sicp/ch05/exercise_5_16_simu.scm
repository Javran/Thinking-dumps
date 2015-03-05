(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "exercise_5_16_simu_tracing_patch.scm")

(load "exercise_5_16_test_controller.scm")

(let ((m (build-with
          test-tracing-controller
          '((n 5))
          (ops-builder-union
           ex-5-16-ops-builder-extra
           default-ops-builder))))
  (machine-trace-on! m)
  (machine-fresh-start! m))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
