(define count-leaves-controller
  '(controller
    ;; whenever we jump back
    ;; using "continue",
    ;; always make sure that "result" register
    ;; stores the correct answer

    (assign continue (label count-done))
    count-loop
    ;; an empty tree
    (test (op null?) (reg tree))
    (branch (label empty-tree))
    ;; not a pair, consider it as a leaf
    (assign result (op pair?) (reg tree))
    (test (op not) (reg result))
    (branch (label leaf))
    ;; else
    ;; do things recursively on two trees
    (save continue)                     ; stack: [continue ..]
    (assign continue (label after-car-tree))
    (save tree)                         ; stack: [tree continue ..]
    (assign tree (op car) (reg tree))
    ;; call (count-leaves (car tree))
    (goto (label count-loop))
    after-car-tree
    (restore tree)                      ; stack: [continue ..]
    (assign continue (label after-cdr-tree))
    (assign tree (op cdr) (reg tree))
    (save result)                       ; stack: [result continue ..]
    (goto (label count-loop))
    after-cdr-tree
    (restore tmp)                       ; stack: [continue ..]
    (restore continue)                  ; stack: <balanced>
    ;; the result of calling (count-leaves (car tree))
    ;; is stored in "tmp",
    ;; and the result of calling (count-leaves (cdr tree))
    ;; is stored in "result", add them up
    (assign result (op +) (reg result) (reg tmp))
    ;; check: result is correct
    (goto (reg continue))

    leaf
    (assign result (const 1))
    ;; check: result = 1 is correct
    (goto (reg continue))

    empty-tree
    (assign result (const 0))
    ;; check: result = 0 is correct
    (goto (reg continue))
    count-done))
