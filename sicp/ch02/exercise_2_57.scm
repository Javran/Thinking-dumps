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
(define (augend s)
  (let ((result (cddr s)))
    (if (= 1 (length result))
      (car result)
      (cons '+ result))))

(define multiplier cadr)
(define (multiplicand p)
  (let ((result (cddr p)))
    (if (= 1 (length result))
      (car result)
      (cons '* result))))

(let ((a (make-sum 1 2 3))
      (b (make-product 4 5 6)))
  (out (addend a)
       (augend a)
       (multiplier b)
       (multiplicand b)))

; test
(out (deriv '(* x 2) 'x))
; = 2

(out (deriv '(* x y (+ x 3)) 'x))
; (+ (* x (+ (* y (+ 1 0)) (* 0 (+ x 3)))) (* 1 (* y (+ x 3))))
; => (+ (* x y) (* y (+ x 3)))
; => (+ (* 2 x y) (* 3 y))

(end-script)
