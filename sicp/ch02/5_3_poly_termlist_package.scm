(define (install-poly-termlist-package)
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

  ; (make-from-args order1 coeff1 order2 coeff2 ...)
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

  (put 'make 'poly-termlist (tagged 'poly-termlist make-empty))
  (put 'make-from-args 'poly-termlist (tagged 'poly-termlist make-from-args))
  (put 'first-term '(poly-termlist) first-term)
  (put 'rest-terms '(poly-termlist) (tagged 'poly-termlist rest-terms))
  (put 'add '(poly-termlist poly-termlist) (tagged 'poly-termlist add-terms))
  (put 'mul '(poly-termlist poly-termlist) (tagged 'poly-termlist mul-terms))
  (put 'empty? '(poly-termlist) empty-termlist?)

  ; extension: fetch a list of term order / coeff
  (put 'order-list '(poly-termlist) order-list)
  (put 'coeff-list '(poly-termlist) coeff-list)

  'done)
