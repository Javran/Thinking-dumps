(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "legacy-easy.scm")
(load "exercise_5_16_legacy_tracing_patch.scm")

(load "exercise_5_16_test_controller.scm")

(let ((m (make-with
          test-tracing-controller
          '((n 5))
          (default-primitive-list))))
  (trace-on! m)
  (start m))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
