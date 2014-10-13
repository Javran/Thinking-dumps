(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./legacy-easy.scm")
(load "./exercise_5_18_legacy_regtrace_patch.scm")

(load "./exercise_5_18_test_controller.scm")

;; the legacy patch uses the approach described
;; in the exercise to implement register value tracing

(let ((m (make-and-execute
          test-controller
          '())))
  (out "done"))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
