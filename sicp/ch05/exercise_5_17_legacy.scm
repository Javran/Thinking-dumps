(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./legacy-easy.scm")
(load "./exercise_5_17_legacy_prelabel_patch.scm")

(load "./figure_5_12.scm")

;; TODO: show that it does not interfere with instruction counting

(let ((m (make-with
          fib-machine-controller
          '((n 5))
          (default-primitive-list))))
  (trace-on! m)
  (start m))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
