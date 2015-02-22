(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "legacy-easy.scm")
(load "exercise_5_15_legacy_insncounter_patch.scm")

(load "figure_5_12.scm")

(let ((m (make-and-execute
          `(controller
            ,@(cdr fib-machine-controller)
            (perform (op print-insn-counter))
            ;; counter + 1
            (perform (op reset-insn-counter))
            ;; = 1
            (perform (op print-insn-counter))
            )
          '((n 5)))))
  (out (get-instruction-counter m)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
