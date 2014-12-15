(define default-ops-builder
  (let ((old-builder default-ops-builder))
    (lambda (m)
      `(,(to-machine-prim-entry 'cond->if)
        ,(to-machine-prim-entry 'let->combination)
        ,@(old-builder m)))))
