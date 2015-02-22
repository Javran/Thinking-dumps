(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "legacy-easy.scm")
(load "exercise_5_17_legacy_prelabel_patch.scm")

(load "figure_5_12.scm")

(let ((m (make-with
          `(controller
            ,@(cdr fib-machine-controller)
            (perform (op print-insn-counter))
            ;; counter + 1 (should be exactly the same as ex 5.15: 166)
            (perform (op reset-insn-counter))
            ;; = 1
            (perform (op print-insn-counter))
            )
          '((n 5))
          (default-primitive-list))))
  (trace-on! m)
  (start m)
  (out (get-instruction-counter m))
  (out (get-register-contents m 'val)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
