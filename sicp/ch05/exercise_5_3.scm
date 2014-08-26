(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_5_5_common.scm")

;; fix x = 144
(define test-const 144)

(define x test-const)
(define (good-enough? guess)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess)
  (average guess (/ x guess)))

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

;; test sqrt-machine-v1
(out "sqrt-machine-v1:")
(ms-pretty-print
 (execute-controller-with-regs
  sqrt-machine-v1
  `((x ,test-const))))
(newline)(newline)

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

(set! good-enough? #f)

;; test sqrt-machine-v2
(out "sqrt-machine-v2:")
(ms-pretty-print
 (execute-controller-with-regs
  sqrt-machine-v2
  `((x ,test-const))))
(newline)(newline)

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

(set! improve #f)
(set! x #f)

;; test sqrt-machine-v3
(out "sqrt-machine-v3:")
(ms-pretty-print
 (execute-controller-with-regs
  sqrt-machine-v3
  `((x ,test-const))))
(newline)(newline)
