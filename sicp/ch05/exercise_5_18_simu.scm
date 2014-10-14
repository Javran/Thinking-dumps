(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./exercise_5_18_simu_regtrace_patch.scm")

(load "./exercise_5_18_test_controller.scm")

;; in this simu.scm patch, register value tracing
;; is implemented in the register value setter.
;; this approach assumes every modification will use
;; that register value setter.
;; It might be not as efficient as the one described
;; in the exercise, since it needs to access to
;; some machine metadata which is not stored inside a register
;; representation. But for the purpose of debugging,
;; I think there won't be much difference between these two approaches.

(let ((m (build-and-execute
          test-controller
          '())))
  (out "done"))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
