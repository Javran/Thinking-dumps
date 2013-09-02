; notice that once we have `first-term` `rest-terms` `empty?`
; we can access the termlist without knowing the underlying type.
; moreover, if we have `adjoin-term`, then `add-terms` becomes possible
; respect this idea, I'll collect some procedure makers here.
; so that when the required methods can be founded, we can make handlers
; with little pain by simply call the procedure to make one for us.
(define (install-poly-generic-package)

  (define (termlist-equ?-maker
            first-term
            rest-terms
            empty?)
    (define (termlist-equ? l1 l2)
      (cond ((and (empty? l1) (empty? l2)) #t)
            ((or  (empty? l1) (empty? l2)) #f)
            (else (and (equ? (first-term l1)
                             (first-term l2))
                       (termlist-equ? (rest-terms l1)
                                      (rest-terms l2))))))
    termlist-equ?)

  (define (add-terms-maker
            first-term
            rest-terms
            empty-termlist?
            adjoin-term)
    (define (add-terms l1 l2)
      (if (empty-termlist? l1)
        l2
        (adjoin-term (first-term l1)
                     (add-terms (rest-terms l1) l2))))
    add-terms)

  (define (mul-term-by-all-terms-maker
            first-term
            rest-terms
            empty-termlist?
            make-empty
            adjoin-term)
    (define (mul-term-by-all-terms t1 l)
      (if (empty-termlist? l)
        (make-empty)
        ; t1*(a1+a2...) = t1*a1+t1*(a2+...)
        (adjoin-term
          (mul 
            t1 (first-term l))
          (mul-term-by-all-terms
            t1 (rest-terms l)))))
    mul-term-by-all-terms)

  (define (mul-terms-maker 
            first-term
            rest-terms
            empty-termlist?
            make-empty
            add-terms
            mul-term-by-all-terms)
    (define (mul-terms l1 l2)
      (if (empty-termlist? l1)
        (make-empty)
        (add-terms (mul-term-by-all-terms
                     (first-term l1) l2)
                   (mul-terms
                     (rest-terms l1) l2))))
    mul-terms)

  (define (neg-terms-maker
            mul-term-by-all-terms)
    (define (neg-terms tl)
      (mul-term-by-all-terms
        (make-term-oc 0 (make-scheme-number -1))
        tl))
    neg-terms)
  
  (define (sub-terms-maker
            add-terms
            neg-terms)
    (define (sub-terms  l1 l2)
      (add-terms l1 (neg-terms l2)))
    sub-terms)

  (define (test-poly-termlist
            termlist-type
            ; suffix '1' to distinguish
            ; bound procedures with its generic counterparts
            make-empty
            make-from-args
            first-term1
            rest-terms1
            adjoin-term
            add-terms
            sub-terms
            mul-term-by-all-terms
            mul-terms
            empty-termlist?
            termlist-equ?)
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
    (define (make-from-intcseq . coeffs)
      (contents (apply make-tl-from-cseq
                       (cons termlist-type
                             (map make-scheme-number coeffs)))))
    (let ((testcases
            (list
              (mat 
                ; 0x + 0 = 0x
                (make-term-oc 1 (make-scheme-number 0))
                (make-empty)
                ; result
                (make-empty))
              (mat
                ; 2x + 0 = 2x
                (make-term-oc 1 (make-scheme-number 2))
                (make-empty)
                ; result
                (make-from-args
                  1 (make-scheme-number 2)))
              (mat
                ; 2x^7 + 3x^2
                (make-term-oc 7 (make-scheme-number 2))
                (make-from-args
                  2 (make-scheme-number 3))
                ; result
                (make-from-args
                  7 (make-scheme-number 2)
                  2 (make-scheme-number 3)))
              (mat
                ; 2x^5 + -2/1 x^5 + 3x^3 + 2x^2
                ; = 3x^3 + 2x^2
                (make-term-oc 5 (make-scheme-number 2))
                (make-from-args
                  5 (make-rational -4 2)
                  3 (make-scheme-number 3)
                  2 (make-scheme-number 2))
                ; result
                (make-from-args
                  3 (make-scheme-number 3)
                  2 (make-scheme-number 2)))
              (mat
                ; 2x^3 + 1x^3 + 2x^2 + 1x^1 + 0x^0
                (make-term-oc 3 (make-scheme-number 2))
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
              (mat
                ; 2x^2 + 6x^6 + 3x^3
                (make-term-oc 2 (make-scheme-number 2))
                (make-from-args
                  6 (make-scheme-number 6)
                  3 (make-scheme-number 3))
                ; result
                (make-from-args
                  6 (make-scheme-number 6)
                  3 (make-scheme-number 3)
                  2 (make-scheme-number 2)))
              (mat (make-term-oc 10 (make-scheme-number 0))
                   (make-from-intcseq 1 2 3 4)
                   ; result
                   (make-from-intcseq 1 2 3 4))
              (mat (make-term-oc 4 (make-scheme-number 7))
                   (make-empty) 
                   ; result
                   (make-from-intcseq 7 0 0 0 0))
              (mat (make-term-oc 4 (make-scheme-number 7)) 
                   (make-from-intcseq 1 2 3)
                   ; result
                   (make-from-intcseq 7 0 1 2 3))
              (mat (make-term-oc 4 (make-scheme-number 7))
                   (make-from-intcseq 1 2 3 4)
                   ; result
                   (make-from-intcseq 7 1 2 3 4))
              (mat (make-term-oc 4 (make-scheme-number 7))
                   (make-from-intcseq 1 2 3 4 5)
                   ; result
                   (make-from-intcseq 8 2 3 4 5))
              (mat (make-term-oc 1 (make-scheme-number 5))
                   (make-from-intcseq 1 2 3 4 5)
                   ; result
                   (make-from-intcseq 1 2 3 9 5))
              (mat (make-term-oc 0 (make-scheme-number 4))
                   (make-from-intcseq 1 2 3 4 5)
                   ; result
                   (make-from-intcseq 1 2 3 4 9))
              (mat (make-term-oc 4 (make-scheme-number -5))
                   (make-from-intcseq 5 0 0 0 0)
                   ; result
                   nil)
              (mat (make-term-oc 1 (make-scheme-number 2))
                   (make-from-intcseq 4 0 0 0)
                   ; result
                   (make-from-intcseq 4 0 2 0))
              (mat (make-term-oc 5 (make-scheme-number 28))
                   (make-from-intcseq 18 0 36 0 0 0)
                   ; result
                   (make-from-intcseq 46 0 36 0 0 0))
              )))
      (do-test-q adjoin-term testcases termlist-equ?))
    ; test add-terms
    (let ((testcases
            (list
              ; 0 + 0 = 0
              (mat (make-empty)
                   (make-empty)
                   ; result
                   (make-empty))
              ; 0 + x = x
              (mat (make-empty)
                   (make-from-intcseq 1 0)
                   ; result
                   (make-from-intcseq 1 0))
              ; x + 0 = x
              (mat (make-from-intcseq 1 0)
                   (make-empty)
                   ; result
                   (make-from-intcseq 1 0))
              (mat (make-from-intcseq
                     ; 3x^2 + 2^x + 1
                     3 2 1)
                   (make-from-intcseq
                     ; -3x^2 - 2^x - 1
                     -3 -2 -1)
                   ; result
                   (make-empty))
              (mat (make-from-intcseq
                     ; x^6 + x^4 + x^2
                     1 0 1 0 1 0 0)
                   (make-from-intcseq
                     ; x^5 + x^3 + x
                     1 0 1 0 1 0)
                   ; result
                   (make-from-intcseq
                     1 1 1 1 1 1 0))
              (mat (make-from-intcseq 3 2 1) (make-from-intcseq 5 4 0 0 0)
                   (make-from-intcseq 5 4 3 2 1))
              (mat (make-from-intcseq 5 4 3 2 1) (make-from-intcseq -5 -4 -3 -2 -1)
                   (make-empty))
              (mat (make-from-intcseq 28 0 0 0 0 0) (make-from-intcseq 18 0 36 0 0 0)
                   (make-from-intcseq 46 0 36 0 0 0))
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
              (mat (make-term-oc 2 (make-scheme-number 4)) 
                   (make-from-intcseq 1 2 3)
                   ; result
                   (make-from-intcseq 4 8 12 0 0))
              (mat (make-term-oc 7 (make-scheme-number 0))
                   (make-from-intcseq 1 2 3 4 5 6)
                   ; result
                   (make-empty))
              (mat (make-term-oc 100 (make-scheme-number 100))
                   (make-empty)
                   ; result
                   (make-empty))
              )))
      (do-test-q mul-term-by-all-terms testcases termlist-equ?))
    ; test mul-terms
    (let ((testcases
            (list
              (mat (make-empty)
                   (make-from-intcseq 100 0)
                   ; result
                   (make-empty))
              (mat (make-from-intcseq 200 0)
                   (make-empty)
                   ; result
                   (make-empty))
              (mat (make-from-intcseq
                     ; 2x + 3
                     2 3)
                   (make-from-intcseq
                     ; 2x - 3
                     2 -3)
                   ; result
                   (make-from-intcseq
                     ; 4x^2 - 9
                     4 0 -9))
              (mat (make-empty) (make-empty)
                   (make-empty))
              (mat (make-empty) (make-from-intcseq 1 2 3 4 5)
                   (make-empty))
              (mat (make-from-intcseq 1 2 3 4 5) (make-empty)
                   (make-empty))
              (mat (make-from-intcseq 1 2 3) (make-from-intcseq 4 5 6)
                   ; (x^2 + 2x + 3) * (4x^2 + 5x + 6)
                   ; => 4x^4 + 13x^3 + 28^x2 + 27x + 18
                   (make-from-intcseq 4 13 28 27 18))
              (mat (make-from-intcseq 5 0 0 7 0 9 0) (make-from-intcseq 2 0 4 0 0)
                   ;(5x^6 + 7x^3 + 9x) * (2x^4 + 4x^2)
                   ; => 10x^10 + 20x^8 + 14x^7 + 46x^5 + 36x^3
                   (make-from-intcseq 10 0 20 14 0 46 0 36 0 0 0))

              )))
      (do-test-q mul-terms testcases termlist-equ?))
    ; test sub-terms
    (let ((testcases
            (list
              (mat (make-from-intcseq 
                     ; 30 x^3 + 20 x^2 + 10 x^1
                     30 20 10 0)
                   (make-from-intcseq
                     ; 15 x^3 + 10 x^2 +  5 x^1
                     15 10  5 0)
                   ; result
                   (make-from-intcseq
                     ; 15 x^3 + 10 x^2 +  5 x^1
                     15 10  5 0))
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

  (put 'termlist-equ?-maker 'poly-generic termlist-equ?-maker)
  (put 'add-terms-maker 'poly-generic add-terms-maker)
  (put 'mul-term-by-all-terms-maker 'poly-generic mul-term-by-all-terms-maker)
  (put 'mul-terms-maker 'poly-generic mul-terms-maker)
  (put 'neg-terms-maker 'poly-generic neg-terms-maker)
  (put 'sub-terms-maker 'poly-generic sub-terms-maker)
  (put 'test-poly-termlist 'poly-generic test-poly-termlist)

  'done)
