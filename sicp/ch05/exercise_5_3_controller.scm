(define sqrt-machine-v1
  '(controller
    ;; the following line is necessary
    ;; in order to make "x" appear in the controller text
    (assign x (reg x))

    (assign guess (const 1.0))
    test-good
    (test (op good-enough?) (reg guess))
    (branch (label iter-done))
    (assign result (op improve) (reg guess))
    (assign guess (reg result))
    (goto (label test-good))
    iter-done
    (assign result (reg guess))))

;; version 2 expands "good-enough?"
(define sqrt-machine-v2
  '(controller
    (assign guess (const 1.0))
    test-good
    ;; "good-enough?" starts here
    (assign result (op square) (reg guess))
    (assign t1 (reg result))
    (assign result (op -) (reg t1) (reg x))
    (assign t1 (reg result))
    (assign result (op abs) (reg t1))
    (test (op <) (reg result) (const 0.001))
    ;; "good-enough?" ends here
    (branch (label iter-done))
    (assign result (op improve) (reg guess))
    (assign guess (reg result))
    (goto (label test-good))
    iter-done
    (assign result (reg guess))))

;; version 3 expands "improve"
(define sqrt-machine-v3
  '(controller
    (assign guess (const 1.0))
    test-good
    ;; "good-enough?" starts here
    (assign result (op square) (reg guess))
    (assign t1 (reg result))
    (assign result (op -) (reg t1) (reg x))
    (assign t1 (reg result))
    (assign result (op abs) (reg t1))
    (test (op <) (reg result) (const 0.001))
    ;; "good-enough?" ends here
    (branch (label iter-done))
    ;; "improve" starts here
    (assign result (op /) (reg x) (reg guess))
    (assign t1 (reg result))
    (assign result (op average) (reg guess) (reg t1))
    ;; "improve" ends here
    (assign guess (reg result))
    (goto (label test-good))
    iter-done
    (assign result (reg guess))))
