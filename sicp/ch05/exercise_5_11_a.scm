(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")

;; since we only need to verify that it works,
;; we use "simu.scm" only

(define fib-machine-controller
  '(controller
    (assign continue (label fib-done))
    fib-loop
    (test (op <) (reg n) (const 2))
    (branch (label immediate-answer))
    ;; update "continue"
    (save continue)
    (assign continue (label afterfib-n-1))
    ;; update "n" -> "n-1"
    (save n)
    (assign n (op -) (reg n) (const 1))
    (goto (label fib-loop))
    afterfib-n-1
    (restore n)
    (restore continue)
    ;; restore "n", do "fib (n-2)"
    (assign n (op -) (reg n) (const 2))
    (save continue)
    (assign continue (label afterfib-n-2))
    (save val)
    ;; "fib (n-1)" goes to stack top and "val"
    (goto (label fib-loop))
    afterfib-n-2
    ;; "val = fib (n-2)"

    ;; ==== these two lines ====
    ;; (assign n (reg val))
    ;; (restore val)
    ;; ==== can be replaced by ====
    (restore n)
    ;; ====
    ;; reason:
    ;;   immediately after the label "afterfib-n-2",
    ;;   register "val" stores the value of "fib (n-2)",
    ;;   and the value of "fib (n-1)" is on the top of the stack
    ;;   we just need to add them up to get the result,
    ;;   and since addition is commutative, it doesn't matter
    ;;   how them get added up.
    ;;   in the original controller, when adding two results up,
    ;;   "n" holds the value of "fib (n-2)" and "val" holds the value of "fib (n-1)"
    ;;   through stack manipulation.
    ;;   but in our new controller, when addting two results up,
    ;;   "n" holds the value of "fib (n-1)" through stack manipulation,
    ;;   and "val" holds the value of "fib (n-2)" through "recursive call",
    ;;   despite that two approach stores value in different manner,
    ;;   they both end up with the correct answer.
    (restore continue)
    (assign val (op +) (reg val) (reg n))
    (goto (reg continue))
    immediate-answer
    (assign val (reg n))
    (goto (reg continue))
    fib-done))

(let ((m (build-and-execute
          fib-machine-controller
          '((n 10)))))
  (out (machine-reg-get m 'val)))
