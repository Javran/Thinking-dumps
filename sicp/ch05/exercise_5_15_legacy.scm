(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./legacy-easy.scm")
(load "./exercise_5_15_legacy_insncounter_patch.scm")

(load "./figure_5_12.scm")

;; TODO: provide some operations
;; to make it possible for the controller-text
;; to interact with the counter

(let ((m (make-and-execute
          fib-machine-controller
          '((n 5)))))
  ;; print out number of instructions executed
  (out (get-instruction-counter m)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
