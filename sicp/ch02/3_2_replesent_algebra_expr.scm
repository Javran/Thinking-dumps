(load "../common/utils.scm")
(load "./3_2_symbolic_diff.scm")

; symbols as variable
(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))

; sum: a list with leading '+
(define (sum? x)
  (and (non-empty? x)
       (eq? (car x) '+)))

; addend: second item
(define (addend s) (cadr s))
; augend: third item
(define (augend s) (caddr s))

; product: a list with leading '*
(define (product? x)
  (and (non-empty? x)
       (eq? (car x) '*)))

; multiplier: second
(define (multiplier p) (cadr p))
; multiplicand: third
(define (multiplicand p) (caddr p))

(out (deriv '(+ x 3) 'x)
     (deriv '(* x y) 'x)
     (deriv '(* (* x y) (+ x 3)) 'x))


(end-script)
