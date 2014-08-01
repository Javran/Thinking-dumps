;; TODO: run if if possible
(define factorial-machine
  '(controller
      (assign product (const 1))
      (assign counter (const 1))
    test-counter-gt-n
      (test (op >) (reg counter) (reg n))
      (branch (label factorial-done))
      (assign result (op *) (reg counter) (reg product))
      (assign t1 (reg result))
      (assign product (reg t1))
      (assign result (op +) (reg counter) (const 1))
      (assign t2 (reg result))
      (assign counter (reg t2))
      (goto (label test-counter-gt-n))
    factorial-done))
