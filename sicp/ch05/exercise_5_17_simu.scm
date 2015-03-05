(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "exercise_5_17_simu_prelabel_patch.scm")

(load "figure_5_12.scm")

(let ((m (build-with
          `(controller
            ,@(cdr fib-machine-controller)
            (perform (op print-insn-counter))
            ;; counter + 1 (should be exactly the same as ex 5.15: 166)
            (perform (op reset-insn-counter))
            ;; = 1
            (perform (op print-insn-counter))
            )
          '((n 5))
          (ops-builder-union
           ex-5-15-ops-builder-extra
           ex-5-16-ops-builder-extra
           default-ops-builder))))
  (machine-trace-on! m)
  (machine-fresh-start! m)
  (out (machine-instruction-counter m))
  (out (machine-reg-get m 'val)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
