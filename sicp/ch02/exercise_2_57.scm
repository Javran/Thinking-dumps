(load "../common/utils.scm")

; symbols as variable
(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; sum: a list with leading '+
(define (sum? x)
  (and (non-empty? x)
       (eq? (car x) '+)))

; product: a list with leading '*
(define (product? x)
  (and (non-empty? x)
       (eq? (car x) '*)))
; excerpt from 2.3.2

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var)
            1
            0))
        ((sum? exp)
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var)))
        ((product? exp)
          (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
        (else
          (error "unknown expression type: DERIV" exp))))
(define (make-sum . ls)
  (cons '+ ls))

(define (make-product . ls)
  (cons '* ls))

(define addend cadr)
(define augend cddr)

(define multiplier cadr)
(define multiplicand cddr)

; test
(out (deriv '(* x (+ 2 y)) 'x)
     (deriv '(* x (+ 2 y)) 'y))
(newline)


(end-script)
