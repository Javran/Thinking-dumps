(define (install-poly-termlist-sparse-package)
  (define (make-empty) nil)
  (define first-term car)
  (define rest-terms cdr)
  (define empty-termlist? null?)

  (define coeff ((curry2 apply-generic) 'coeff))
  (define order ((curry2 apply-generic) 'order))

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

    (let ((terms (map pair-to-term
                      (list-to-pair-list args))))
      (fold-right adjoin-term (make-empty) terms)))

  (define (adjoin-term term term-list)
    (cond ((=zero? term)
            ; case #1
            term-list)
          ; term is non-zero
          ((empty-termlist? term-list)
            ; case #2
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
                      ; case #3
                      ; if t-order > ft-order, insert in front of the termlist
                      (cons term term-list))
                    ((= t-order ft-order)
                      ; if t-order = ft-order
                      (let ((result-term (add ft term)))
                        ; coeff = 0
                        (if (=zero? result-term)
                          ; case #4
                          ; drop the first term
                          (rest-terms term-list)
                          ; case #5
                          ; else
                          (cons result-term
                                (rest-terms term-list)))))
                    ((< t-order ft-order)
                      ; case #6
                      ; if t-order < ft-order
                      (cons (first-term term-list)
                           (adjoin-term term (rest-terms term-list))))
                    (else (error "impossible case")))))))

  ; join two term-lists
  (define add-terms
    ((get 'add-terms-maker 'poly-generic)
     first-term
     rest-terms
     empty-termlist?
     adjoin-term))

  (define mul-term-by-all-terms
    ((get 'mul-term-by-all-terms-maker 'poly-generic)
     first-term
     rest-terms
     empty-termlist?
     make-empty
     adjoin-term))

  (define mul-terms
    ((get 'mul-terms-maker 'poly-generic)
     first-term
     rest-terms
     empty-termlist?
     make-empty
     add-terms
     mul-term-by-all-terms))

  (define neg-terms
    ((get 'neg-terms-maker 'poly-generic)
     mul-term-by-all-terms))

  (define sub-terms
    ((get 'sub-terms-maker 'poly-generic)
     add-terms
     neg-terms))
  
  (define termlist-equ?
    ((get 'termlist-equ?-maker 'poly-generic)
     first-term
     rest-terms
     empty-termlist?))

  (define div-terms
    ((get 'div-terms-maker 'poly-generic)
     first-term
     rest-terms
     empty-termlist?
     make-empty
     sub-terms
     mul-term-by-all-terms))

  (define (test)
    ((get 'test-poly-termlist 'poly-generic)
     'poly-termlist-sparse
     make-empty
     make-from-args
     first-term
     rest-terms
     adjoin-term
     add-terms
     sub-terms
     mul-term-by-all-terms
     mul-terms
     empty-termlist?
     termlist-equ?))

  (put 'make 'poly-termlist-sparse (tagged 'poly-termlist-sparse make-empty))
  (put 'make-from-args 'poly-termlist-sparse (tagged 'poly-termlist-sparse make-from-args))
  (put 'first-term '(poly-termlist-sparse) first-term)
  (put 'rest-terms '(poly-termlist-sparse) (tagged 'poly-termlist-sparse rest-terms))
  (put 'add '(poly-termlist-sparse poly-termlist-sparse) (tagged 'poly-termlist-sparse add-terms))
  (put 'sub '(poly-termlist-sparse poly-termlist-sparse) (tagged 'poly-termlist-sparse sub-terms))
  (put 'mul '(poly-termlist-sparse poly-termlist-sparse) (tagged 'poly-termlist-sparse mul-terms))
  (put 'div '(poly-termlist-sparse poly-termlist-sparse)
       (lambda (l1 l2)
         (map ((curry2 attach-tag) 'poly-termlist) (div-terms l1 l2))))

  (put 'empty? '(poly-termlist-sparse) empty-termlist?)
  (put '=zero? '(poly-termlist-sparse) empty-termlist?)
  (put 'equ? '(poly-termlist-sparse poly-termlist-sparse) termlist-equ?)

  (put 'test 'poly-termlist-sparse-package test)

  'done)
