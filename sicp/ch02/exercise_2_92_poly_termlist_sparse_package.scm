(define (install-poly-termlist-sparse-package)
  (define (make-empty) nil)
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
     termlist-equ?)
    ; test add-terms
    ; TODO: can be a generic add-terms test
    (let ((testcases
            (list
              ; 0 + 0 = 0
              (mat (make-empty)
                   (make-empty)
                   ; result
                   (make-empty))
              ; 0 + x = x
              (mat (make-empty)
                   (make-from-args
                     1 (make-scheme-number 1))
                   ; result
                   (make-from-args
                     1 (make-scheme-number 1)))
              ; x + 0 = x
              (mat (make-from-args
                     1 (make-scheme-number 1))
                   (make-empty)
                   ; result
                   (make-from-args
                     1 (make-scheme-number 1)))
              (mat (make-from-args
                     ; 3x^2 + 2^x + 1
                     2 (make-scheme-number 3)
                     1 (make-scheme-number 2)
                     0 (make-scheme-number 1))
                   (make-from-args
                     ; -3x^2 - 2^x - 1
                     2 (make-scheme-number -3)
                     1 (make-scheme-number -2)
                     0 (make-scheme-number -1))
                   ; result
                   (make-empty))
              (mat (make-from-args
                     ; x^6 + x^4 + x^2
                     6 (make-scheme-number 1)
                     4 (make-scheme-number 1)
                     2 (make-scheme-number 1))
                   (make-from-args
                     ; x^5 + x^3 + x
                     5 (make-scheme-number 1)
                     3 (make-scheme-number 1)
                     1 (make-scheme-number 1))
                   ; result
                   (make-from-args
                     6 (make-scheme-number 1)
                     5 (make-scheme-number 1)
                     4 (make-scheme-number 1)
                     3 (make-scheme-number 1)
                     2 (make-scheme-number 1)
                     1 (make-scheme-number 1)))
              )))
      (do-test-q add-terms testcases termlist-equ?))
    ; test mul-term-by-all-terms
    (let ((testcases
            (list
              (mat (make-term-oc 0 (make-scheme-number 2))
                   (make-from-args
                     3 (make-scheme-number 3)
                     2 (make-rational 4 5)
                     1 (make-complex-ri 1 2))
                   ; result
                   (make-from-args
                     3 (make-scheme-number 6)
                     2 (make-rational 8 5)
                     1 (make-complex-ri 2 4)))
              (mat (make-term-oc 10 (make-scheme-number 0))
                   (make-from-args
                     1 (make-scheme-number 1))
                   ; result
                   (make-empty))
              (mat (make-term-oc 2 (make-rational 1 2))
                   (make-from-args
                     2 (make-scheme-number 4)
                     4 (make-scheme-number 8))
                   ; result
                   (make-from-args
                     4 (make-scheme-number 2)
                     6 (make-scheme-number 4)))
              (mat (make-term-oc 2 (make-rational 1 2))
                   (make-empty)
                   ; result
                   (make-empty))
              )))
      (do-test-q mul-term-by-all-terms testcases termlist-equ?))
    ; test mul-terms
    (let ((testcases
            (list
              (mat (make-empty)
                   (make-from-args 1 (make-scheme-number 100))
                   ; result
                   (make-empty))
              (mat (make-from-args 1 (make-scheme-number 200))
                   (make-empty)
                   ; result
                   (make-empty))
              (mat (make-from-args
                     ; 2x + 3
                     1 (make-scheme-number 2)
                     0 (make-scheme-number 3))
                   (make-from-args
                     ; 2x - 3
                     1 (make-scheme-number 2)
                     0 (make-scheme-number -3))
                   ; result
                   (make-from-args
                     ; 4x^2 - 9
                     2 (make-scheme-number 4)
                     0 (make-scheme-number -9)))
              )))
      (do-test-q mul-terms testcases termlist-equ?))
    ; test neg-terms
    (let ((testcases
            (list
              (mat (make-from-args 1 (make-scheme-number 100))
                   (make-from-args 1 (make-scheme-number -100)))
              (mat (make-empty)
                   (make-empty))
              )))
      (do-test-q neg-terms testcases termlist-equ?))
    ; test sub-terms
    (let ((testcases
            (list
              (mat (make-from-args 
                     1 (make-scheme-number 10)
                     2 (make-scheme-number 20)
                     3 (make-scheme-number 30))
                   (make-from-args
                     1 (make-scheme-number  5)
                     2 (make-scheme-number 10)
                     3 (make-scheme-number 15))
                   ; result
                   (make-from-args
                     1 (make-scheme-number  5)
                     2 (make-scheme-number 10)
                     3 (make-scheme-number 15)))
              (mat (make-empty)
                   (make-empty)
                   ; result
                   (make-empty))
              (mat (make-empty)
                   (make-from-args
                     3 (make-rational 4 5)
                     6 (make-complex-ri 7 8))
                   ; result
                   (make-from-args
                     3 (make-rational -4 5)
                     6 (make-complex-ri -7 -8)))
              )))
      (do-test-q sub-terms testcases termlist-equ?))
    )

  (put 'make 'poly-termlist-sparse (tagged 'poly-termlist-sparse make-empty))
  (put 'make-from-args 'poly-termlist-sparse (tagged 'poly-termlist-sparse make-from-args))
  (put 'first-term '(poly-termlist-sparse) first-term)
  (put 'rest-terms '(poly-termlist-sparse) (tagged 'poly-termlist-sparse rest-terms))
  (put 'add '(poly-termlist-sparse poly-termlist-sparse) (tagged 'poly-termlist-sparse add-terms))
  (put 'sub '(poly-termlist-sparse poly-termlist-sparse) (tagged 'poly-termlist-sparse sub-terms))
  (put 'mul '(poly-termlist-sparse poly-termlist-sparse) (tagged 'poly-termlist-sparse mul-terms))
  (put 'empty? '(poly-termlist-sparse) empty-termlist?)
  (put '=zero? '(poly-termlist-sparse) empty-termlist?)
  (put 'order-list '(poly-termlist-sparse) order-list)
  (put 'coeff-list '(poly-termlist-sparse) coeff-list)
  (put 'equ? '(poly-termlist-sparse poly-termlist-sparse) termlist-equ?)

  (put 'test 'poly-termlist-sparse-package test)

  'done)
