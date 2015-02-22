(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "legacy-easy.scm")
(load "exercise_5_19_legacy_breakpoint_patch.scm")

(load "figure_5_12.scm")

(let ((m (make-with
          fib-machine-controller
          '((n 2))
          (default-primitive-list))))
  (trace-on! m)
  ;; will reach breakpoint at "(branch (label immediate-answer))"
  (set-breakpoint m 'fib-loop 2)
  (start m)
  ;; proceed first time, will stop at the same point in future
  (proceed-machine m)
  (cancel-all-breakpoints m)
  ;; run until it terminates
  (proceed-machine m)
  (out (get-register-contents m 'val))
  'done)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
