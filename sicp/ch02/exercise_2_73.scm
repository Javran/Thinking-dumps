(load "../common/utils.scm")

(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))
(define (make-sum a1 a2)
  (list '+ a1 a2))
(define (make-product m1 m2)
  (list '* m1 m2))
(define (sum? x)
  (and (non-empty? x) (eq? (car x) '+)))
(define (product? x)
  (and (non-empty? x) (eq? (car x) '*)))
(define addend cadr)
(define augend caddr)
(define multiplier cadr)
(define multiplicand caddr)

(define (deriv-1 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var) 1 0))
        ((sum? exp)
          (make-sum (deriv-1 (addend exp) var)
                    (deriv-1 (augend exp) var)))
        ((product? exp)
          (make-sum (make-product
                      (multiplier exp)
                      (deriv-1 (multiplicand exp) var))
                    (make-product
                      (deriv-1 (multiplier exp) var)
                      (multiplicand exp))))
        ; (more rules can be added here)
        (else (error "unknown expression type: DERIV-1" exp))))

(out (deriv-1 '(+ x 3) 'x))
(newline)

(load "./4_3_data_directed_put_get.scm")

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
                (operands exp) var))))
(define operator car)
(define operands cdr)

; a. Explain what was done above. Why can’t we assimilate the pred-
; icates number? and variable? into the data-directed dispatch?

; answer: symbols like '+ and '* are regarded as tags
;   so that `deriv` can be re-written into data-directed style.
;   and number? as well as variable? cannot be assimilated simply because
;   that numbers and variables do not have special tags,
;   which is required for the procedure "operator".

(load "./exercise_2_73_impl.scm")
(install-deriv-my-impl)

(out (deriv '(* x (+ x (* 3 y))) 'x))
(out (deriv '(* x (+ x (* 3 y))) 'y))
; x * (x + (3 * y))
; => x*x + 3*x*y
; (deriv by x) => 2*x + 3*y
; result: 
; (+ (* 1 (+ x (* 3 y))) (* (+ 1 (+ (* 0 y) (* 0 3))) x))
; => (+ (+ x (* 3 y))  x)
; => x + x + 3*y
; (deriv by y) => 3*x
; result:
; (+ (* 0 (+ x (* 3 y))) (* (+ 0 (+ (* 0 y) (* 1 3))) x))
; => (* 3 x)
; => 3*x

(end-script)
