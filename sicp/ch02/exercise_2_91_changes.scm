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

  (define (test)
    (let ((a (make-from-args
               5 (make-scheme-number 1)
               0 (make-scheme-number -1)))
          (b (make-from-args
               2 (make-scheme-number 1)
               0 (make-scheme-number -1))))
      (out (div-terms a b)))
      )

  (put 'make 'poly-termlist (tagged 'poly-termlist make-empty))
  (put 'make-from-args 'poly-termlist (tagged 'poly-termlist make-from-args))
  (put 'first-term '(poly-termlist) first-term)
  (put 'rest-terms '(poly-termlist) (tagged 'poly-termlist rest-terms))
  (put 'add '(poly-termlist poly-termlist) (tagged 'poly-termlist add-terms))
  (put 'sub '(poly-termlist poly-termlist) (tagged 'poly-termlist sub-terms))
  (put 'mul '(poly-termlist poly-termlist) (tagged 'poly-termlist mul-terms))
  (put 'neg '(poly-termlist) (tagged 'poly-termlist neg-terms))
  (put 'empty? '(poly-termlist) empty-termlist?)
  (put 'test 'poly-termlist-package-2 test)

  (put 'order-list '(poly-termlist) order-list)
  (put 'coeff-list '(poly-termlist) coeff-list)

  'done)

(install-poly-termlist-package-2)
