(define my-append-controller
  ;; input reg: x y
  ;; output reg: result
  `(controller
    (assign continue (label append-done))

    append-loop
    (test (op null?) (reg x))
    ;; "x" is null
    (branch (label x-is-null))
    ;; "x" is not null
    (save continue)                     ; stack: [continue ..]
    (assign tmp-1 (op car) (reg x))
    (save tmp-1)                        ; stack: [(car x) continue ..]
    (assign x (op cdr) (reg x))
    ;; destruct "x", and call "append" recursively
    (assign continue (label after-append-1))
    (goto (label append-loop))
    after-append-1
    (restore tmp-1)                     ; stack: [continue ..]
    ;; after the recursive call,
    ;; "result" keeps the return value of (append (cdr x) y)
    ;; which we can use directly
    (assign result (op cons) (reg tmp-1) (reg result))
    (restore continue)                  ; stack: <balanced>
    (goto (reg continue))

    x-is-null
    (assign result (reg y))
    (goto (reg continue))

    append-done))

(define my-last-pair-controller
  ;; input: x (a list)
  ;; output: result
  `(controller
    (assign continue (label last-pair-done))

    last-pair-loop
    (assign tmp-1 (op cdr) (reg x))
    ;; test (null? (cdr x))
    (test (op null?) (reg tmp-1))
    (branch (label last-pair-found))
    ;; else do it recursively on (cdr x)
    (assign x (reg tmp-1))
    (save continue) ; stack: [continue ..]
    (assign continue (label last-pair-after-rec-call))
    (goto (label last-pair-loop))
    last-pair-after-rec-call
    ;; the final result is already stored in "result"
    (restore continue) ; stack: <balanced>
    (goto (reg continue))

    last-pair-found
    (assign result (reg x))
    (goto (reg continue))

    last-pair-done))
