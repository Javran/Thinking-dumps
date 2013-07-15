(load "../common/utils.scm")

; symbols as variable
(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

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
        ((exponentiation? exp)
          (let* ((u (base exp))
                 (n (exponent exp))
                 (du/dx (deriv u var)))
            (make-product
              (make-product
                n
                (make-exponentiation u (- n 1)))
              du/dx)))
        (else
          (error "unknown expression type: DERIV" exp))))


; test
(out (deriv '(* x (+ 2 y)) 'x)
     (deriv '(* x (+ 2 y)) 'y))
(newline)

(define (exponentiation? e)
  (and (non-empty? e)
       (eq? (car e) '**)))

(define base cadr)
(define exponent caddr)

(define (make-exponentiation e1 e2)
  (cond ((and (number? e2)
              (= e2 0)) 1)
        ((and (number? e2)
              (= e2 1)) e1)
        (else (list '** e1 e2))))

(out (deriv '(** x 6) 'x)
     ; =6x^5
     ; (x^4+y^5)*2
     (deriv '(* (+ (** x 4) (** y 5)) 2) 'x)
     ; =8x^3
     (deriv '(* (+ (** x 4) (** y 5)) 2) 'y)
     ; =10y^4
     )

(end-script)
