(load "../common/utils.scm")

(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(load "./4_3_data_directed_put_get.scm")

; index the procedures in the opposite way
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv)
                (operands exp) var))))
(define operator car)
(define operands cdr)

; answer:
; we should rearrange the argument of `put` 
;   to keep its correspondence with new definition of `get`:
;   i.e. (get type op) <=> (put type op item)
; another change is: all calls to `get` should be changed accordingly as well

; ----------- change #1:
(define (put type op item)
  (set! proc-table (put-proc op type item proc-table)))

(load "./exercise_2_73_impl.scm")
(install-deriv-my-impl)
; ----------- change #2:
(define make-sum (get 'sum-product 'make-sum))
; ----------- change #3:
(define make-product (get 'sum-product 'make-product))

(out (deriv '(* x (+ x (* 3 y))) 'x))
(out (deriv '(* x (+ x (* 3 y))) 'y))
(newline)


(load "./exercise_2_73_exp_impl.scm")
(install-deriv-my-exp-impl)
; ----------- change #4:
(define make-exponentiation (get 'exp 'make-exponentiation))

(out (deriv '(** x 6) 'x)
     ; =6x^5
     ; (x^4+y^5)*2
     (deriv '(* (+ (** x 4) (** y 5)) 2) 'x)
     ; =8x^3
     (deriv '(* (+ (** x 4) (** y 5)) 2) 'y)
     ; =10y^4
     )

(end-script)
