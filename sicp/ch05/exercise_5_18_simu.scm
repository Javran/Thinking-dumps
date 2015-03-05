(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "exercise_5_18_simu_regtrace_patch.scm")

(load "exercise_5_18_test_controller.scm")

;; I was trying to take the second approach described in
;; "exercise_5_18.md", but that approach turns out to be
;; too more complicated than expected. So I go back to
;; the suggested approach instead.

(let ((m (build-and-execute-with
          test-controller
          '()
          (ops-builder-union
           ex-5-18-ops-builder-extra
           default-ops-builder))))
  (out "done"))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
