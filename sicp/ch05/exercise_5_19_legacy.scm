(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./legacy-easy.scm")
(load "./exercise_5_19_legacy_breakpoint_patch.scm")

(load "./figure_5_12.scm")

(let ((m (make-with
          fib-machine-controller
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
