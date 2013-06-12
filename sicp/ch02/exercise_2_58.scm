(load "../common/utils.scm")

; symbols as variable
(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; get notation for infix representation
(define (notation s)
  (if (and (list? s)
           (= (length s) 3))
    (cadr s)
    #f))

; sum: a list with leading '+
(define (sum? x)
  (eq? (notation x) '+))

; product: a list with leading '*
(define (product? x)
  (eq? (notation x) '*))

(define (make-sum s1 s2)
  (list s1 '+ s2))

(define (make-product p1 p2)
  (list p1 '* p2))

(out (sum? (make-sum 1 2))
     ; #t 
     (product? (make-product 10 4))
     ; #t
     (sum? 1)
     ; #f
     )

(define addend car)
(define augend caddr)

(define multiplier car)
(define multiplicand caddr)

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
(end-script)
