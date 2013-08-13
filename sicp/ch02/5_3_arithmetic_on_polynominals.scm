(load "../common/utils.scm")
(load "../common/test-utils.scm")
(load "./tag_system.scm")
(load "./number_system.scm")

(load "./5_3_polynominal_package.scm")
(install-polynomial-package)

(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (equal? v1 v2)))

(load "./5_3_poly_term_package.scm")
(load "./5_3_poly_termlist_package.scm")
(install-poly-term-package)
(install-poly-termlist-package)

(define make-term (get 'make 'poly-term))
(define (order x) (apply-generic 'order x))
(define (coeff x) (apply-generic 'coeff x))

(run-test 'poly-term-package)

(define (make-poly v t)
  ((get 'make 'polynominal) v t))

; t1 = 3x^2
; t2 = 4x^1
; t3 = 2x^0
; t4 = 2x^1
;(let ((t1 (adjoin-term (make-term 2 (make-scheme-number 3)) (the-empty-termlist)))
;      (t2 (adjoin-term (make-term 1 (make-scheme-number 4)) (the-empty-termlist)))
;      (t3 (adjoin-term (make-term 0 (make-scheme-number 2)) (the-empty-termlist)))
;      (t4 (adjoin-term (make-term 1 (make-scheme-number 2)) (the-empty-termlist))))
;  (let ((l1 (add-terms t1 t2))
;        (l2 (add-terms t3 t4)))
;    ; l1 = 3x^2+4x
;    ; l2 = 2x+2
;    (out (add-terms l1 l2))
;    (out (mul-terms l1 l2))
;    ))
;
; t1 = 3x^2
; t2 = (2+3i)x^1
; t3 = 7x^0
; t4 = x^4
; t5 = 2/3 x^2
; t6 = (5+3i)x^0
;(let ((t1 (adjoin-term (make-term 2 (make-scheme-number 3)) (the-empty-termlist)))
;      (t2 (adjoin-term (make-term 1 (make-complex-ri 2 3)) (the-empty-termlist)))
;      (t3 (adjoin-term (make-term 0 (make-scheme-number 7)) (the-empty-termlist)))
;      (t4 (adjoin-term (make-term 4 (make-scheme-number 1)) (the-empty-termlist)))
;      (t5 (adjoin-term (make-term 2 (make-rational 2 3)) (the-empty-termlist)))
;      (t6 (adjoin-term (make-term 0 (make-complex-ri 5 3)) (the-empty-termlist))))
;  (let ((l1 (add-terms (add-terms t1 t2) t3))
;        (l2 (add-terms (add-terms t4 t5) t6)))
;    (let ((p1 (make-poly 'x l1))
;          (p2 (make-poly 'x l2)))
;      (out p1
;           p2
;           (add p1 p2)
;           (mul p1 p2)))))

(out ((get 'make-from-args 'poly-termlist)
       1 (make-scheme-number 5)
       1 (make-scheme-number 5)
       3 (make-rational 2 3)))

(end-script)
