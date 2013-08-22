(define (install-poly-termlist-sparse-package)
  (define (make-empty) nil)
  (define the-empty-termlist make-empty)
  (define first-term car)
  (define rest-terms cdr)
  (define empty-termlist? null?)

  (define coeff ((curry2 apply-generic) 'coeff))
  (define order ((curry2 apply-generic) 'order))

  ; extension: fetch a list of term order / coeff
  (define coeff-list ((curry2 map) coeff))
  (define order-list ((curry2 map) order))

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
    (cond ((=zero? term)
            term-list)
          ; term is non-zero
          ((empty-termlist? term-list)
            (list term))
          ; term-list is non-empty
          (else 
            (let* ((ft (first-term term-list))
                   (ft-order (order ft))
                   (ft-coeff (coeff ft))
                   (t-order (order term))
                   (t-coeff (coeff term)))
              ; bind ft-* -> first term in the termlist
              (cond ((> t-order ft-order)
                      ; if t-order > ft-order, insert in front of the termlist
                      (cons term term-list))
                    ((= t-order ft-order)
                      ; if t-order = ft-order
                      (let ((result-term (add ft term)))
                        ; coeff = 0
                        (if (=zero? result-term)
                          ; drop the first term
                          (rest-terms term-list)
                          ; else
                          (cons result-term
                                (rest-terms term-list)))))
                    ((< t-order ft-order)
                      ; if t-order < ft-order
                      (cons (first-term term-list)
                           (adjoin-term term (rest-terms term-list))))
                    (else (error "impossible case")))))))

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
  
  ; TODO: should be a package-independent procedure
  ; bind concrete impl of first-term, rest-terms and empty?
  ; make the `equ?` handler
  (define (termlist-equ?-maker first-term rest-terms empty?)
    (define (termlist-equ? l1 l2)
      (cond ((and (empty? l1) (empty? l2)) #t)
            ((or  (empty? l1) (empty? l2)) #f)
            (else (and (equ? (first-term l1)
                             (first-term l2))
                       (termlist-equ? (rest-terms l1)
                                      (rest-terms l2))))))
    termlist-equ?)

  (define termlist-equ?
    (termlist-equ?-maker first-term rest-terms empty-termlist?))

  (define (test)
    ; test make-from-args
    ;   try to avoid using equ? here because
    ;   this moment it has not been well tested
    ; use an exact structure comparison
    (let ((l1 (make-from-args
                1 (make-scheme-number 1)
                3 (make-scheme-number 3)
                5 (make-scheme-number 5)))
          (l2 (make-from-args
                3 (make-scheme-number 3)
                5 (make-scheme-number 5)
                1 (make-scheme-number 1)))
          )
     (do-test-q (rec-eq? eq?)
               (list (mat l1 l2 #t)))))

  (put 'make 'poly-termlist-sparse (tagged 'poly-termlist-sparse make-empty))
  (put 'make-from-args 'poly-termlist-sparse (tagged 'poly-termlist-sparse make-from-args))
  (put 'first-term '(poly-termlist-sparse) first-term)
  (put 'rest-terms '(poly-termlist-sparse) (tagged 'poly-termlist-sparse rest-terms))
  (put 'add '(poly-termlist-sparse poly-termlist-sparse) (tagged 'poly-termlist-sparse add-terms))
  (put 'mul '(poly-termlist-sparse poly-termlist-sparse) (tagged 'poly-termlist-sparse mul-terms))
  (put 'empty? '(poly-termlist-sparse) empty-termlist?)
  (put '=zero? '(poly-termlist-sparse) empty-termlist?)
  (put 'order-list '(poly-termlist-sparse) order-list)
  (put 'coeff-list '(poly-termlist-sparse) coeff-list)
  (put 'equ? '(poly-termlist-sparse poly-termlist-sparse) termlist-equ?)

  (put 'test 'poly-termlist-sparse-package test)

  'done)
