;; lx-addr: '(layer-index . in-layer-position)
(define (lexical-address-lookup lx-addr env)
  (let ((layer-i (car lx-addr))
        (pos-i   (cdr lx-addr)))
    (list-ref
     (cdr (list-ref env layer-i)) pos-i)))
