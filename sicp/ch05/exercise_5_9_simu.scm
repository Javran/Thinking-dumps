(define (make-operation-exp prim-exp arg-exps m)
  (let ((op (machine-lookup-prim
             m prim-exp))
        (aprocs
         (map (lambda (e)
                (if (tagged-list? e 'label)
                    (error "cannot operate on labels")
                    (make-primitive-exp e m)))
              arg-exps)))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))
