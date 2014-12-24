;; modified from "../ch04/4_2_2_representing_thunks.scm"

(define (delay-it exp env)
  `(thunk ,exp ,env))

(define thunk?
  (list-tagged-with 'thunk))

(define thunk-exp cadr)
(define thunk-env caddr)

(define evaluated-thunk?
  (list-tagged-with 'evaluated-thunk?))

(define thunk-value cadr)

(define (thunk-set-value! obj value)
  (assert (thunk? obj)
          "the object is not an unevaluated thunk")
  (set-car! obj 'evaluated-thunk?)
  (set-car! (cdr obj) value)
  (set-cdr! (cdr obj) '()))
