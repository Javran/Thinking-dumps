(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "exercise_5_15_simu_insncounter_patch.scm")

(load "figure_5_12.scm")

(let ((m (build-and-execute
          `(controller
            ,@(cdr fib-machine-controller)
            (perform (op print-insn-counter))
            ;; counter + 1
            (perform (op reset-insn-counter))
            ;; = 1
            (perform (op print-insn-counter))
            )
          '((n 5)))))
  (out (machine-instruction-counter m)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
