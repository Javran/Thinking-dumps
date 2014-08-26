(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_5_5_common.scm")

(define fib-machine-controller-mod
  '(controller
    (assign continue (label fib-done))
    fib-loop
    (test (op <) (reg n) (const 2))
    (branch (label immediate-answer))
    (save continue)
    (assign continue (label afterfib-n-1))
    (save n)
    (assign n (op -) (reg n) (const 1))
    (goto (label fib-loop))
    afterfib-n-1
    (restore n)
    ;; (restore continue)
    (assign n (op -) (reg n) (const 2))
    ;; (save continue)
    (assign continue (label afterfib-n-2))
    (save val)
    (goto (label fib-loop))
    afterfib-n-2
    (assign n (reg val))
    (restore val)
    (restore continue)
    (assign val (op +) (reg val) (reg n))
    (goto (reg continue))
    immediate-answer
    (assign val (reg n))
    (goto (reg continue))
    fib-done))

(ms-pretty-print
 (execute-controller-with-regs
  fib-machine-controller-mod
  '((n 5))))

;; the extra `restore` and `save` instructions
;; have been commented out

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
