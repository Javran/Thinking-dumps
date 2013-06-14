(load "../common/utils.scm")

; symbols as variable
(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

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

; we need to implement `make-sum` `sum?` `addend` `augend`
;   and `make-product` `product?` `multiplier` `multiplicand`

; for sum:
; find anything that contains `+`
; apply the pattern: {addend} + {augend}
(define (sum? ls)
  (and (list? ls)
       (> (length ls) 1)
       (if (memq '+ ls)
         #t
         #f)))

(define (cut-at x ls)
  ; given that x is one of the element of ls
  ;   returns a pair that
  ;     the first element is everything before x
  ;   and the second element is everything after x
  (if (null? ls)
    (cons '() '())
    (let ((head (car ls))
          (tail (cdr ls)))
      (if (eq? head x)
        (cons '() tail)
        (let ((sub-result (cut-at x tail)))
          (cons (cons head (car sub-result))
                (cdr sub-result)))))))

(define (cut-at2 x ls)
  ; same as cut-at, but the returned pair will have something like (1) converted to 1
  (define (get x)
    (if (and (list? x)
             (= (length x) 1))
      (car x)
      x))
  (let ((result (cut-at x ls)))
    (cons (get (car result))
          (get (cdr result)))))

(define (addend ls)
  (car (cut-at2 '+ ls)))

(define (augend ls)
  (cdr (cut-at2 '+ ls)))

(let ((x '(1 * x * (2 + y) + 3 + z)))
  (out (addend x)
       (augend x)))

(define (make-sum a b)
  (list a '+ b))

; for product: 
; anything that does not contain `+` (given that the expression is valid)
(define (product? ls)
  (and (list? ls)
       (> (length ls) 1)
       (if (memq '+ ls)
         #f
         #t)))

(define multiplier car)
(define (multiplicand x)
  (define (get x)
    (if (and (list? x)
             (= (length x) 1))
      (car x)
      x))
  (get (cddr x)))

(define (make-product a b)
  (list a '* b))

(out (deriv '(x + 3 * (x + y + 2)) 'x))
; = 4
(out (deriv '(x + 3 * (x + y + 2)) 'y))
; = 3

(end-script)
