(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")

(define (lexical-address-lookup lx-addr env)
  (let ((layer-i (car lx-addr))
        (pos-i   (cdr lx-addr)))
    (list-ref (list-ref env layer-i) pos-i)))

(define test-env
  (extend-environment
   '(a b c d)
   '(a b c d)
   (extend-environment
    '(z y x)
    '(z y x)
    (extend-environment
     '(e f g h)
     '(e f g h)
     the-empty-environment))))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
