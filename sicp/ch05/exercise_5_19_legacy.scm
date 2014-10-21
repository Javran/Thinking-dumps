(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./legacy-easy.scm")
(load "./exercise_5_19_legacy_breakpoint_patch.scm")

(load "./figure_5_12.scm")

(let ((m (make-with
          fib-machine-controller
          '((n 2))
          (default-primitive-list))))
  (trace-on! m)
  ;; will reach breakpoint at "(branch (label immediate-answer))"
  (set-breakpoint m 'fib-loop 2)
  (start m))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
