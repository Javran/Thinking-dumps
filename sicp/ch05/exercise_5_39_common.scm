;; lookup variable in the environment using lexical addressing
;; lx-addr: '(layer-index . in-layer-position)
;; error will be signalled if the value of the variable is *unassigned*
(define (lexical-address-lookup lx-addr env)
  (define (lexical-address-lookup-intern lx-addr env)
    (let ((layer-i (car lx-addr))
          (pos-i   (cdr lx-addr)))
      (list-ref
       (cdr (list-ref env layer-i)) pos-i)))
  (let ((result (lexical-address-lookup-intern lx-addr env)))
    (assert (not (eq? result '*unassigned*))
            "the value of the variable is not yet assigned")
    result))
