(define test-tracing-controller
  '(controller
    ;; input: register n expects a number
    p-begin
    (test (op zero?) (reg n))
    (branch (label p-done))
    (test (op odd?) (reg n))
    (branch (label p-trace-on))
    (perform (op trace-off))
    (perform (op print) (reg n))
    (goto (label p-loop-next))
    p-trace-on
    (perform (op trace-on))
    (perform (op print) (reg n))
    p-loop-next
    (assign n (op -) (reg n) (const 1))
    (goto (label p-begin))
    p-done))
