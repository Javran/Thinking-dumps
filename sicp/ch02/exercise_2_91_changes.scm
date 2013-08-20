; codes are copied from ./5_3_poly_termlist_package.scm
; and ./exercise_2_88_changes.scm
; 
; in order to implement div-terms,
; we need to use hidden tools like `mul-term-by-all-terms`

(define (install-poly-termlist-package-2)
  (define (make-empty) nil)
  (define the-empty-termlist make-empty)
  (define first-term car)
  (define rest-terms cdr)
  (define empty-termlist? null?)

  (define coeff ((curry2 apply-generic) 'coeff))
  (define order ((curry2 apply-generic) 'order))

  (define coeff-list
    ((curry2 map) coeff))
  (define order-list
    ((curry2 map) order))

  (define (make-from-args . args)
    (define (list-to-pair-list ls)
      (cond ((null? ls) '())
            ((>= (length ls) 2)
              (cons 
                (cons (car ls) (cadr ls))
                (list-to-pair-list (cddr ls))))
            (else
              (error "invalid list length"))))
    (define (pair-to-term p)
      ((get 'make 'poly-term) (car p) (cdr p)))
    (define (term-to-termlist t)
      (adjoin-term t (make-empty)))

    (let ((terms (map (compose
                        term-to-termlist
                        pair-to-term)
                      (list-to-pair-list args))))
      (fold-left add-terms (make-empty) terms)))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

  (define (add-terms l1 l2)
    (cond ((empty-termlist? l1) l2)
          ((empty-termlist? l2) l1)
          (else
            (let ((t1 (first-term l1))
                  (t2 (first-term l2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms l1) l2)))
                    ((< (order t1) (order t2))
                     (adjoin-term 
                       t2 (add-terms l1 (rest-terms l2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms l1)
                                   (rest-terms l2)))))))))

  (define (mul-terms l1 l2)
    (if (empty-termlist? l1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term l1) l2)
                 (mul-terms (rest-terms l1) l2))))

  (define (mul-term-by-all-terms t1 l)
    (if (empty-termlist? l)
      (the-empty-termlist)
      (let ((t2 (first-term l)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms l))))))

  (define (neg-terms tl)
    (let ((neg-one (make-term 0 (make-scheme-number -1))))
      (mul-term-by-all-terms neg-one tl)))

  (define (sub-terms l1 l2)
    (add-terms l1 (neg-terms l2)))

  ; ==== begin solution ====
  (define (div-terms l1 l2)
    (if (empty-termlist? l1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term l1))
            (t2 (first-term l2)))
        (if (> (order t2) (order t1))
          (list (the-empty-termlist) l1)
          (let ((new-c (div (coeff t1) (coeff t2)))
                (new-o (-   (order t1) (order t2))))
            (let ((rest-of-result
                    (div-terms 
                      (sub-terms l1 
                                 (mul-term-by-all-terms
                                   (make-term new-o new-c)
                                   l2))
                      l2)))
              (let ((terms-quotient (car rest-of-result))
                    (terms-remainder (cadr rest-of-result)))
                (list (adjoin-term (make-term new-o new-c)
                                   terms-quotient)
                      terms-remainder))))))))

  (define (equ-termlist? l1 l2)
    (empty-termlist? (sub-terms l1 l2)))

  (define (test)
    (let ((a (make-from-args
               5 (make-scheme-number 1)
               0 (make-scheme-number -1)))
          (b (make-from-args
               2 (make-scheme-number 1)
               0 (make-scheme-number -1)))
          (q (make-from-args
               3 (make-scheme-number 1)
               1 (make-scheme-number 1)))
          (r (make-from-args
               1 (make-scheme-number 1)
               0 (make-scheme-number -1)))
          (l1 (make-from-args
                ; 1/2 x^5 + 3/4 x^4 + 5/6 x^3 + 7/8 x^2 + 9/10 x + 10/11
                5 (make-rational  1  2)
                4 (make-rational  3  4)
                3 (make-rational  5  6)
                2 (make-rational  7  8)
                1 (make-rational  9 10)
                0 (make-rational 10 11)))
          (l2 (make-from-args
                ; 3 x^3 + 7 x^2 - 9
                3 (make-scheme-number 3)
                2 (make-scheme-number 7)
                0 (make-scheme-number -9)))
          (l3 (make-from-args
                ; 8 x^2 - 4 x + 2
                2 (make-scheme-number 8)
                1 (make-scheme-number -4)
                0 (make-scheme-number 2)))
          )
      (let ((testcases
              (list
                ; a = b*q + r
                (mat a b 
                     (list q r))
                (mat (add-terms (mul-terms l1 l2)
                                l3)
                     l2
                     (list l1 l3))
                ))
            (result-eq?
              (lambda (l1 l2)
                (apply boolean/and
                       (map equ-termlist? l1 l2)))))
        (do-test-q div-terms testcases result-eq?)
      )))

  (put 'make 'poly-termlist (tagged 'poly-termlist make-empty))
  (put 'make-from-args 'poly-termlist (tagged 'poly-termlist make-from-args))
  (put 'first-term '(poly-termlist) first-term)
  (put 'rest-terms '(poly-termlist) (tagged 'poly-termlist rest-terms))
  (put 'add '(poly-termlist poly-termlist) (tagged 'poly-termlist add-terms))
  (put 'sub '(poly-termlist poly-termlist) (tagged 'poly-termlist sub-terms))
  (put 'mul '(poly-termlist poly-termlist) (tagged 'poly-termlist mul-terms))
  (put 'div '(poly-termlist poly-termlist)
       ; special case: div-terms returns a list of poly-termlist rather than the poly-termlist itself
       (lambda (l1 l2)
         (map ((curry2 attach-tag) 'poly-termlist) (div-terms l1 l2))))

  (put 'equ? '(poly-termlist poly-termlist) equ-termlist?)
  (put 'neg '(poly-termlist) (tagged 'poly-termlist neg-terms))
  (put 'empty? '(poly-termlist) empty-termlist?)
  (put 'test 'poly-termlist-package-2 test)

  (put 'order-list '(poly-termlist) order-list)
  (put 'coeff-list '(poly-termlist) coeff-list)

  'done)

(define (install-polynomial-package-div)
  (define make-poly cons)
  (define variable car)
  (define term-list cdr)

  (define (div-poly p1 p2)
    (assert (same-variable?
              (variable p1)
              (variable p2)))
    (let ((result (div (term-list p1)
                       (term-list p2))))
      (map (lambda (tl)
             (attach-tag 'polynominal
                         (make-poly (variable p1)
                                    tl)))
           result)))

  (put 'div '(polynominal polynominal) div-poly)

  )

(install-poly-termlist-package-2)
(install-polynomial-package-div)
