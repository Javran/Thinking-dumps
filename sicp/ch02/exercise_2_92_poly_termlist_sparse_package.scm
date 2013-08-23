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
  (define (add-terms l1 l2)
    (fold-right adjoin-term l1 l2))

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
          (make-term-oc (+ (order t1) (order t2))
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
                (list (mat l1 l2 #t))))
    ; test termlist-equ?
    (let ((l1 (make-from-args
                1 (make-scheme-number 2)
                3 (make-scheme-number 4)))
          (l2 (make-from-args
                1 (make-complex-ri 2 0)
                3 (make-rational 8 2)))
          (l3 (make-empty))
          (l4 (make-from-args
                2 (make-scheme-number 2)))
          )
      (let ((testcases
              (list
                (mat l1 l2 #t)
                (mat l1 l3 #f)
                (mat l1 l4 #f)
                (mat l2 l3 #f)
                (mat l2 l4 #f)
                (mat l3 l3 #t)
                (mat l3 l4 #f))))
        (do-test-q termlist-equ? testcases)))
    ; test adjoin-term
    ;   make-from-args uses adjoin-term implicitly
    ;   so it does make a simple test for adjoin-term
    ;   but here we try to cover all possible situations
    ;   to make us more confident.
    (let ((testcases
            (list
              ; case #1
              (mat (make-term-oc 1 (make-scheme-number 0))
                   (make-empty)
                   ; result
                   (make-empty))
              ; case #2
              (mat (make-term-oc 1 (make-scheme-number 2))
                   (make-empty)
                   ; result
                   (make-from-args
                     1 (make-scheme-number 2)))
              ; case #3
              (mat (make-term-oc 7 (make-scheme-number 2))
                   (make-from-args
                     2 (make-scheme-number 3))
                   ; result
                   (make-from-args
                     7 (make-scheme-number 2)
                     2 (make-scheme-number 3)))
              ; case #4
              (mat (make-term-oc 5 (make-scheme-number 2))
                   (make-from-args
                     5 (make-rational -4 2)
                     3 (make-scheme-number 3)
                     2 (make-scheme-number 2))
                   ; result
                   (make-from-args
                     3 (make-scheme-number 3)
                     2 (make-scheme-number 2)))
              ; case #5
              (mat (make-term-oc 3 (make-scheme-number 2))
                   (make-from-args
                     3 (make-scheme-number 1)
                     2 (make-scheme-number 2)
                     1 (make-scheme-number 1)
                     0 (make-scheme-number 0))
                   ; result
                   (make-from-args
                     3 (make-scheme-number 3)
                     2 (make-scheme-number 2)
                     1 (make-scheme-number 1)))
              ; case #6
              (mat (make-term-oc 2 (make-scheme-number 2))
                   (make-from-args
                     6 (make-scheme-number 6)
                     3 (make-scheme-number 3))
                   ; result
                   (make-from-args
                     6 (make-scheme-number 6)
                     3 (make-scheme-number 3)
                     2 (make-scheme-number 2)))
                   )))
      (do-test-q adjoin-term testcases termlist-equ?))
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

    
    )

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
