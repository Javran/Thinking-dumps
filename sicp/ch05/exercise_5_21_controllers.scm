(define count-leaves-controller
  '(controller
    (assign continue (label count-done))
    count-loop
    ;; empty tree
    (test (op null?) (reg tree))
    (branch (label empty-tree))
    ;; leaf
    (assign result (op pair?) (reg tree))
    (test (op not) (reg result))
    (branch (label leaf))
    ;; else
    ;; (count-leaves (car tree))
    (save continue)
    (assign continue (label after-car-tree))
    (save tree)
    (assign tree (op car) (reg tree))
    (goto (label count-loop))
    after-car-tree
    (restore continue)
    (restore tree)
    (save continue)
    (assign continue (label after-cdr-tree))
    (assign tree (op cdr) (reg tree))
    (save result)
    (goto (label count-loop))
    after-cdr-tree
    (restore continue)
    (restore tmp)
    (assign result (op +) (reg result) (reg tmp))
    (goto (reg continue))

    leaf
    (assign result (const 1))
    (goto (reg continue))

    empty-tree
    (assign result (const 0))
    (goto (reg continue))
    count-done))
