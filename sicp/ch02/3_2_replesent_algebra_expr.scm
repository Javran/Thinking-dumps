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

(define (do-test)
  (out (deriv '(+ x 3) 'x)
       (deriv '(* x y) 'x)
       (deriv '(* (* x y) (+ x 3)) 'x)))

(do-test)

; redefine make-sum and make-product hoping to simplify the result
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(newline)
(do-test)

(newline)
(out (deriv 'x 'x)
     ; 1
     (deriv '(* x x) 'x)
     ; = (* x 2)
     (deriv '(* (* x x) x) 'x)
     ; = (* 3 (* x x))
     )

(end-script)
