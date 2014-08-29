(define (make-operation-exp exp m)
  (let ((op (machine-lookup-prim
             m (operation-exp-op exp)))
        (aprocs
         (map (lambda (e)
                (if (tagged-list? e 'label)
                    (error "cannot operate on labels")
                    (make-primitive-exp e m)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))
