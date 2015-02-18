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

(define (list-set! ls k value)
  (cond ((< k 0) 'ok)
        ((null? ls) 'ok)
        ;; from now on, the list contains
        ;; at least one element.
        ((= k 0) (set-car! ls value))
        (else (list-set! (cdr ls) (sub1 k) value))))

(define (lexical-address-set! lx-addr env new-val)
  ;; TODO
  (error 'todo))
