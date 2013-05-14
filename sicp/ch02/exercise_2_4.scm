(load "../common/utils.scm")

(define (cons-1 x y)
  ; reads "accepts a m, then apply x & y to m"
  (lambda (m) (m x y)))

(define (car-1 z)
  ; reads "applys z by a function that accepts p & q and returns p"
  (z (lambda (p q) p)))

; the corresponding definition of "cdr":
(define (cdr-1 z)
  (z (lambda (p q) q)))

(define a (cons-1 123 456))

(out (car-1 a)
     (cdr-1 a))
; 123
; 456

; to verify that it works:
; a
; => (cons 123 456)
; => (lambda (m) (m 123 456))
; (car a)
; => (a (lambda (p q) p))
; => ((lambda (m) (m 123 456)) (lambda (p q) p))
; => ((lambda (p q) p) 123 456)
; => 123
