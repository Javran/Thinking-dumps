(define expt-machine-1
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

(define expt-machine-2
  '(controller
    (assign counter (reg n))
    (assign product (const 1))
    (assign continue (label expt-done))
    expt-loop
    (test (op =) (reg counter) (const 0))
    (branch (label expt-immediate))
    (assign result (op -) (reg counter) (const 1))
    (assign counter (reg result))
    (assign result (op *) (reg b) (reg product))
    (assign product (reg result))
    (save continue)
    (assign continue (label after-expt))
    (goto (label expt-loop))
    after-expt
    (restore continue)
    (goto (reg continue))

    expt-immediate
    (assign result (reg product))
    (goto (reg continue))

    expt-done))

(define n 5)
(define b (expt 123456 (/ 1 n)))
