;; modified from "../ch04/4_2_2_representing_thunks.scm"

;; generate unique thunk flag so that
;; the implemented language won't be able to resemble one
(define thunk-sym (gensym))
(define evaluated-thunk-sym (gensym))

(define (delay-it exp env)
  `(,thunk-sym ,exp ,env))

(define thunk?
  (list-tagged-with thunk-sym))

(define thunk-exp cadr)
(define thunk-env caddr)

(define evaluated-thunk?
  (list-tagged-with evaluated-thunk-sym))

(define thunk-value cadr)

;; no "force-it" function here
;; as on this level, we don't know how the implemented language
;; is going to evaluate it.
;; therefore the only we can do is to define this "thunk-set-value!"
;; procedure so that when an expression gets evaluated, we can turn an
;; unevaluated thunk into an evaluated one
(define (thunk-set-value! obj value)
  (assert (thunk? obj)
          "the object is not an unevaluated thunk")
  (set-car! obj evaluated-thunk-sym)
  (set-car! (cdr obj) value)
  (set-cdr! (cdr obj) '()))
