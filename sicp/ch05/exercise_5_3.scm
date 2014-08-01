(define sqrt-machine-v1
  '(controller
    (assign guess (const 1.0))
    test-good
    (test (op good-enough?) (reg guess))
    (branch (label iter-done))
    (assign result (op improve) (reg guess))
    (assign guess (reg result))
    (goto (label test-good))
    iter-done
    (assign result (reg guess))))
