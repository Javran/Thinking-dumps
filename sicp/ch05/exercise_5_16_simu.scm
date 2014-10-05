(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./exercise_5_16_simu_tracing_patch.scm")

(load "./figure_5_12.scm")

;; TODO: make trace controllable through
;; instructions

(define test-tracing-controller
  '(controller
    ;; input: register n expects a number
    p-begin
    (test (op zero?) (reg n))
    (branch (label p-done))
    (test (op odd?) (reg n))
    (branch (label p-trace-on))
    ;; (perform (op trace-off))
    (goto (label p-loop-next))
    p-trace-on
    ;; (perform (op trace-on))
    p-loop-next
    (assign n (op -) (reg n) (const 1))
    (goto (label p-begin))
    p-done))

(let ((m (build-with fib-machine-controller
                     '((n 5))
                     default-ops-builder)))
  (machine-trace-on! m)
  (machine-fresh-start! m))

(out "===")

(let ((m (build-with test-tracing-controller
                     '((n 5))
                     default-ops-builder)))
  (machine-trace-on! m)
  (machine-fresh-start! m))


(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
