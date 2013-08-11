(load "../common/utils.scm")
(load "../common/test-utils.scm")
(load "./tag_system.scm")
(load "./number_system.scm")

(load "./5_3_polynominal_package.scm")
(install-polynomial-package)

(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (equal? v1 v2)))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
    term-list
    (cons term term-list)))
(define (the-empty-termlist) nil)
(define first-term car)
(define rest-terms cdr)
(define empty-termlist? null?)

(define make-term list)
(define order car)
(define coeff cadr)

; join two term-lists
(define (add-terms l1 l2)
  (cond ((empty-termlist? l1) l2)
        ((empty-termlist? l2) l1)
        (else
          (let ((t1 (first-term l1))
                (t2 (first-term l2)))
            ; terms of the bigger order always are placed
            ;   in front of lower ones
            (cond ((> (order t1) (order t2))
                    (adjoin-term
                      t1 (add-terms (rest-terms l1) l2)))
                  ((< (order t1) (order t2))
                    (adjoin-term 
                      t2 (add-terms l1 (rest-terms l2))))
                  ; of the same order, merge t1 and t2
                  (else
                    (adjoin-term
                      (make-term (order t1)
                                 (add (coeff t1) (coeff t2)))
                      (add-terms (rest-terms l1)
                                 (rest-terms l2)))))))))
(define (mul-terms l1 l2)
  (if (empty-termlist? l1)
    ; 0 * x = 0
    (the-empty-termlist)
    ; (a1+a2+...)*l2 = a1*l2 + (a2+...)*l2
    (add-terms (mul-term-by-all-terms (first-term l1) l2)
               (mul-terms (rest-terms l1) l2))))

(define (mul-term-by-all-terms t1 l)
  (if (empty-termlist? l)
    (the-empty-termlist)
    ; t1*(a1+a2...) = t1*a1+t1*(a2+...)
    (let ((t2 (first-term l)))
      (adjoin-term
        (make-term (+ (order t1) (order t2))
                   (mul (coeff t1) (coeff t2)))
        (mul-term-by-all-terms t1 (rest-terms l))))))

(define (make-poly v t)
  ((get 'make 'polynominal) v t))

; t1 = 3x^2
; t2 = 4x^1
; t3 = 2x^0
; t4 = 2x^1
(let ((t1 (adjoin-term (make-term 2 (make-scheme-number 3)) (the-empty-termlist)))
      (t2 (adjoin-term (make-term 1 (make-scheme-number 4)) (the-empty-termlist)))
      (t3 (adjoin-term (make-term 0 (make-scheme-number 2)) (the-empty-termlist)))
      (t4 (adjoin-term (make-term 1 (make-scheme-number 2)) (the-empty-termlist))))
  (let ((l1 (add-terms t1 t2))
        (l2 (add-terms t3 t4)))
    ; l1 = 3x^2+4x
    ; l2 = 2x+2
    (out (add-terms l1 l2))
    (out (mul-terms l1 l2))
    ))

(end-script)
