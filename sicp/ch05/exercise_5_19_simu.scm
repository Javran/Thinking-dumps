(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./exercise_5_19_simu_breakpoint_patch.scm")

(load "./figure_5_12.scm")

(let ((m (build-with
          fib-machine-controller
          '((n 10))
          default-ops-builder)))
  (machine-trace-on! m)
  ;; the breakpoint is set on the 2nd instruction
  ;; after "fib-loop" label, which is "(branch (label immediate-answer))"
  ;; and the expected behavior is the machine stopped at the breakpoint
  ;; while the next instruction text gets printed
  (machine-set-breakpoint! m 'fib-loop 2)
  (machine-fresh-start! m))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
