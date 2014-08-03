(define expt-machine
  '(controller
    (assign continue (label expt-done))
    expt-loop
    (test (op =) (reg n) (const 0))
    (branch (label expt-immediate))

    (assign result (op -) (reg n) (const 1))
    (assign n (reg result))
    (save continue)
    (assign continue (label after-expt))
    ;; call expt b (n-1)
    (goto (label expt-loop))
    after-expt
    (assign t1 (reg result))
    ;; calc (* b (expt b (- n 1)))
    (assign result (op *) (reg b) (reg t1))
    (restore continue)
    (goto (reg continue))

    expt-immediate
    (assign result (const 1))
    (goto (reg continue))
    expt-done))
