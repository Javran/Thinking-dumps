(define my-append-controller
  ;; input reg: x y
  ;; output reg: result
  `(controller
    (assign continue (label append-done))
    append-loop

    (test (op null?) (reg x))
    (branch (label x-is-null))

    (save continue)                     ; stack: [continue ..]
    (assign tmp-1 (op car) (reg x))
    (save tmp-1)                        ; stack: [(car x) continue ..]
    (assign x (op cdr) (reg x))
    (assign continue (label after-append-1))
    (goto (label append-loop))
    after-append-1
    (restore tmp-1)                     ; stack: [continue ..]
    (assign result (op cons) (reg tmp-1) (reg result))
    (restore continue)                  ; stack: <balanced>
    (goto (reg continue))

    x-is-null
    (assign result (reg y))
    (goto (reg continue))

    append-done))
