(define count-leaves-controller
  ;; input reg: tree
  ;; output reg: result
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

(define count-leaves-iter-controller
  ;; input reg: tree
  ;; output reg: result
  '(controller
    (assign n (const 0))
    ;; now go into count-iter
    (assign continue (label count-done))
    count-loop
    ;; an empty tree
    (test (op null?) (reg tree))
    (branch (label empty-tree))
    ;; not a pair, consider it as a leaf
    (assign tmp-1 (op pair?) (reg tree))
    (test (op not) (reg tmp-1))
    (branch (label leaf))
    ;; else
    ;; prepare for calling (count-iter (car tree) n)
    (save continue) ; stack: [continue ..]
    (assign continue (label after-car-tree))
    (save tree) ; stack: [tree continue ..]
    (assign tree (op car) (reg tree))
    (goto (label count-loop))
    after-car-tree
    ;; prepare for calling (count-iter (cdr tree) <reg result>)
    (restore tree) ; stack: [continue ..]
    (assign tree (op cdr) (reg tree))
    (assign n (reg result))
    (assign continue (label after-cdr-tree))
    (goto (label count-loop))
    after-cdr-tree
    (restore continue) ; stack: <balanced>
    ;; check: result is correct
    (goto (reg continue))

    leaf
    (assign result (op +) (reg n) (const 1))
    ;; check: result = n+1
    (goto (reg continue))

    empty-tree
    (assign result (reg n))
    ;; check: result = n
    (goto (reg continue))
    count-done))
