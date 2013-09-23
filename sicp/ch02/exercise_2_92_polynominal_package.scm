(define (install-polynomial-package)
  (define make-poly cons)
  (define variable car)
  (define term-list cdr)

  (define tagged-make-poly (tagged 'polynominal make-poly))

  (define (wildcard? v) (eq? 'wildcard v))

  (define (compatible-variable? v1 v2)
    ; two variables are compatible when wildcard exists
    ; or when two variables are exactly the same
    (or (or (wildcard? v1) (wildcard? v2))
        (same-variable? v1 v2)))

  ; merge two variables together
  ; all wildcard -> wildcard
  ; 1 wildcard -> replaced with another one
  (define (merge-variable v1 v2)
    (cond ((same-variable? v1 v2) v1)
          ((wildcard? v1) v2)
          ((wildcard? v2) v1)
          (else (error "impossible case"))))

  ; verify variable equality before performing a binary op, say `f`
  (define (variable-verify f)
    (define (wrap-poly outer-var p)
      (make-poly
        outer-var
        (make-tl-from-args
          'poly-termlist-sparse
          0 (attach-tag 'polynominal p))))
    (lambda (p1 p2)
      (if (compatible-variable?  (variable p1) (variable p2))
        (f p1 p2)
        ; p1 and p2 are not compatible,
        ; in this case, neither p1 nor p2 are not wildcards
        ; we should determine which one should be put inside
        (cond ((variable-less?  (variable p1) (variable p2))
                (f p1 (wrap-poly (variable p1) p2)))
              ((variable-less?  (variable p2) (variable p1))
                (f (wrap-poly (variable p2) p1) p2))
              (else
                (error "unexpected case"))))))

  ; operations without variable verification
  ;   convert arguments between different termlist types on demand
  (define (binary-op-poly-maker f)
    (define (binary-op-poly p1 p2)
      (let* ((tl1 (term-list p1))
             ; convert tl2 to the same type of tl1
             (tl2 (to-poly-termlist-type
                    (term-list p2)
                    (type-tag tl1)))
             (var (merge-variable (variable p1)
                                  (variable p2))))
        (make-poly var (f tl1 tl2))))
    binary-op-poly)

  ; operations with verification
  (define add-poly (variable-verify (binary-op-poly-maker add)))
  (define mul-poly (variable-verify (binary-op-poly-maker mul)))
  (define sub-poly (variable-verify (binary-op-poly-maker sub)))
  
  ; division without verification
  (define (div-poly-no-v p1 p2)
    (let* ((tl1 (term-list p1))
           (tl2 (to-poly-termlist-type
                    (term-list p2)
                    (type-tag tl1)))
           (result (div tl1 tl2)))
      (map (lambda (tl)
             (attach-tag 'polynominal
                         (make-poly (variable p1)
                                    tl)))
           result)))
  ; wt: with tag
  (define div-poly-wt (variable-verify div-poly-no-v))

  (define (to-string-poly p)
    (define (term-to-string term var)
      (string-append 
        "("
        (to-string (coeff term))
        ")"
        (symbol->string var)
        "^"
        (number->string (order term))))
    (let ((var (variable p))
          (termls (term-list p)))
      (cond ((empty? termls) "0")
            ((empty? (rest-terms termls))
              ; only one element
              (term-to-string (first-term termls) var))
            (else
              (string-append
                (term-to-string (first-term termls) var)
                "+"
                (to-string-poly (make-poly var (rest-terms termls))))))))

  (define poly-zero?
    (compose =zero? term-list))

  (define (poly-equ?-nover p1 p2)
    (let* ((tl1 (term-list p1))
           (tl2 (to-poly-termlist-type
                  (term-list p2)
                  (type-tag tl1))))
      (equ? tl1 tl2)))

  (define poly-equ? (variable-verify poly-equ?-nover))

  ; determine variable extraction ordering
  (define (extract-order p)
    ; fetch all variables from a polynominal
    (define (fetch-variables p)
      (cons (variable p)
            (concat
              (filter
                identity
                (map-poly-term
                  (lambda (term)
                    (if (is-poly? (coeff term))
                      (fetch-variables (contents (coeff term)))
                      #f))
                  (term-list p))))))
    (let* ((all-vars (fetch-variables p))
           (all-var-set
             ; remove duplicate members
             (fold-left
               (lambda (ls cur-ele)
                 (if (memq cur-ele ls)
                   ls
                   (cons cur-ele ls)))
               nil
               all-vars)))
      (sort all-var-set
            (lambda (s1 s2)
              (string<? (symbol->string s1)
                        (symbol->string s2))))))

  ; extract `target-var` from term `term` whose variable is `t-var`
  (define (extract-term target-var term t-var)
    (define (wrap-term term-var term-order term-coeff)
      (tagged-make-poly
        term-var
        (make-tl-from-args
          'poly-termlist-sparse
          term-order term-coeff)))
    ; binding: 
    ; term variable: t-var
    ; term coeff   : t-coeff
    ; term order   : t-order
    (let ((t-coeff (coeff term))
          (t-order (order term)))
      (if (is-poly? t-coeff)
        ; coeff is a poly - need further extraction
        (if (same-variable? target-var t-var)
          ; if the embeded coeff has the same variable as the term itself
          (let ((result (extract-poly target-var t-coeff))
                (order-poly (wrap-term target-var t-order (make-scheme-number 1))))
            ; "move" the outer order into the result
            (mul result order-poly))
          ; else
          (let* ((result (extract-poly target-var t-coeff))
                 ; wrapped term
                 (wrapped (wrap-term t-var t-order (make-scheme-number 1)))
                 (order-poly (wrap-term target-var 0 wrapped)))
            ; add up t-order into the poly
            (mul result order-poly)))
        ; else
        ; coeff is a number
        ; make term wrapped in a polynominal
        (let ((wrapped (wrap-term t-var t-order t-coeff)))
          (if (same-variable? target-var t-var)
            wrapped
            ; not same
            (wrap-term target-var 0 wrapped))))))

  (define (extract-poly target-var poly)
    (let* ((var (variable (contents poly)))
           (tl (term-list (contents poly)))
           (terms (map
                    make-term-oc 
                    (order-list tl)
                    (coeff-list tl)))
           (result (map
                     (lambda (term)
                       (extract-term
                         target-var
                         term
                         var))
                     terms))
           )
      ; http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Reduction-of-Lists.html
      ;   The argument initial is used only if list is empty;
      ;   in this case initial is the result of the call to reduce-left.
      ;   If list has a single argument, it is returned.
      ;   Otherwise, the arguments are reduced in a left-associative fashion. 
      (reduce-left 
        add
        (tagged-make-poly
          target-var
          (make-tl-empty 'poly-termlist-sparse))
        result)))

  ; do extraction according to sym-order
  (define (do-extract p sym-order)
    (define (list-of-term->term-list lot)
      (let ((list-of-tl
              (map (lambda (term)
                     (make-tl-from-args
                       'poly-termlist-sparse
                       (order term) (coeff term)))
                   lot)))
        (fold-left
          add
          (make-tl-empty
            'poly-termlist-sparse)
          list-of-tl)))
    (if (null? sym-order)
      p
      ; call do-extract recursively on all terms
      (let ((extract-1 (extract-poly (car sym-order) p)))
        (tagged-make-poly
          (variable (contents extract-1))
          (list-of-term->term-list
            (map-poly-term
              (lambda (term)
                (let ((t-coeff (coeff term)))
                  (if (is-poly? t-coeff)
                    (make-term-oc
                      (order term)
                      (do-extract t-coeff (cdr sym-order)))
                    term)))
              (term-list (contents extract-1))))))))

  (define (extract-all p)
    (let ((ord-list (extract-order (contents p))))
      (do-extract p ord-list)))

  (define (project x)
    (let ((tl (term-list x)))
      (if (empty? tl)
        (make-complex-ri 0 0)
        ((raise-to 'complex)
                  (coeff (first-term tl))))))

  (define (drop-coeffs p)
    (define (try-drop term)
      (let ((t-coeff (coeff term))
            (t-order (order term)))
        (if (is-poly? t-coeff)
          (make-term-oc t-order (drop-coeffs t-coeff))
          (make-term-oc t-order (drop t-coeff)))))
    (define (term->termlist term)
      (make-tl-from-args
        'poly-termlist-sparse
        (order term) (coeff term)))
    (assert (is-poly? p))
    (let* ((poly (contents p))
           (tl (term-list poly))
           ; a list of dropped terms
           (dropped-terms
              (map-poly-term 
                try-drop
                tl))
           (new-tl
             (fold-left
               add
               (make-tl-empty
                 'poly-termlist-sparse)
               (map term->termlist dropped-terms))))
      (tagged-make-poly
        (variable (contents p))
        new-tl)))

  (define (test)
    ; test accessors
    (let* ((obj (make-poly 'stub-var 'stub-terml))
           (testcases (list (mat variable 'stub-var)
                            (mat term-list 'stub-terml)))
           (f (lambda (proc) (proc obj))))
      (do-test-q f testcases))
    ; test poly-zero?
    (let ((testcases
            (list
              (mat (make-poly 
                     'x
                     (make-tl-from-args 
                       'poly-termlist-sparse
                       ; x + 1
                       1 (make-scheme-number 1)
                       0 (make-scheme-number 1)))
                   #f)
              (mat (make-poly
                     'x
                     (make-tl-empty
                       'poly-termlist-sparse))
                   #t)
              (mat (make-poly
                     'x
                     (make-tl-from-args
                       'poly-termlist-sparse
                       2 (make-scheme-number 0)
                       1 (make-scheme-number 0)
                       0 (make-scheme-number 0)))
                   #t)
              )))
      (do-test-q poly-zero? testcases))
    ; test equ-poly?
    (let ((testcases
            (list
              ; all are empty
              (mat (make-poly 'x
                              (make-tl-empty 'poly-termlist-sparse))
                   (make-poly 'x
                              (make-tl-empty 'poly-termlist-sparse))
                   #t)
              ; empty vs non-empty
              (mat (make-poly 'y
                              (make-tl-empty 'poly-termlist-sparse))
                   (make-poly 'y
                              (make-tl-from-cseq-num
                                'poly-termlist-sparse
                                1 2 3 4))
                   #f)
              ; trivial case
              (mat (make-poly 'x
                              (make-tl-from-cseq-num
                                'poly-termlist-sparse
                                1 2 3 0 0 0 4 5 6))
                   (make-poly 'x
                              (make-tl-from-cseq-num
                                'poly-termlist-sparse
                                1 2 3 0 0 0 4 5 6))
                   #t)
              (mat (make-poly 'x
                              (make-tl-from-cseq-num
                                'poly-termlist-sparse
                                1 2 3 4 5 6))
                   (make-poly 'x
                              (make-tl-from-cseq-num
                                'poly-termlist-sparse
                                1 2 2 4 5 6))
                   #f)
              )))
      (do-test-q poly-equ? testcases))
    ; test extract-related functions
    ; * extract-all (used in subtraction, implicitly)
    ; * extract-poly (used to extract given variable out)
    ; test poly:
    ;   (2y^2-y+1)*x^2 + (y+1)*x + 1
    (let* ((p (tagged-make-poly
                'x
                (make-tl-from-args
                  'poly-termlist-sparse
                  0 (make-scheme-number 1)
                  1 (tagged-make-poly
                      'y
                      (make-tl-from-cseq-num
                        'poly-termlist-sparse
                        1 1))
                  2 (tagged-make-poly
                      'y
                      (make-tl-from-cseq-num
                        'poly-termlist-sparse
                        2 -1 1)))))
           (p-x (extract-poly 'x p))
           (p-y (extract-poly 'y p)))
      (assert (same-variable? 'x (variable (contents p-x)))
              "variable not extracted")
      (assert (same-variable? 'y (variable (contents p-y)))
              "variable not extracted")
      (assert (=zero? (extract-all (sub p-x p)))
              "extraction result mismatch")
      (assert (=zero? (extract-all (sub p-y p)))
              "extraction result mismatch")
      )
  
    ; (big-case) will run a big testcase and spend a little longer time
    ; (x^2+2x+3)(y^2+2y+3)(z^2+2z+3)
    ; 6 different ways to expand
    ; separate into 2 groups
    ; append (+x+y+z) in the first group
    ; append (-x-y-z) in the second group
    ; subtraction result should be 2x+2y+2z
    ; TODO:
    ; * define simplify
    (define (big-case)
      (let* (
             (make-test-poly
               ; 3 variables ar v1, v2, v3, additional num
               ;   will be appended into the outmost, 1-order term
               (lambda (v1 v2 v3 addition)
                 (let* ((p1 (tagged-make-poly
                              v1
                              (make-tl-from-cseq-num
                                'poly-termlist-sparse
                                1 2 3)))
                        (p2 (tagged-make-poly
                              v2
                              (make-tl-from-args
                                'poly-termlist-sparse
                                2 p1
                                1 (mul (make-scheme-number 2)
                                       p1)
                                0 (mul (make-scheme-number 3)
                                       p1))))
                        (p3 (tagged-make-poly
                              v3
                              (make-tl-from-args
                                'poly-termlist-sparse
                                2 p2
                                1 (add (mul (make-scheme-number 2) p2)
                                       (make-scheme-number addition))
                                0 (mul (make-scheme-number 3)
                                       p2)))))
                   p3)))
             (c1 (make-test-poly 'x 'y 'z  1))
             (c2 (make-test-poly 'x 'z 'y  1))
             (c3 (make-test-poly 'y 'x 'z -1))
             (c4 (make-test-poly 'y 'z 'x  1))
             (c5 (make-test-poly 'z 'x 'y -1))
             (c6 (make-test-poly 'z 'y 'x -1))
             (result (sub (add c1 (add c2 c4))
                          (add c3 (add c5 c6))))
             )
        (out "==== before" (to-string result))
        (out "==== after"  (to-string (drop-coeffs (simplify result))))
        ))
    ; uncoment next line for full extraction tests
    (big-case)
    )

  (put 'make 'polynominal tagged-make-poly)

  (put 'variable '(polynominal) variable)
  (put 'term-list '(polynominal) term-list)

  (put 'add '(polynominal polynominal) (tagged 'polynominal add-poly))
  (put 'mul '(polynominal polynominal) (tagged 'polynominal mul-poly))
  (put 'sub '(polynominal polynominal) (tagged 'polynominal sub-poly))
  (put 'div '(polynominal polynominal) div-poly-wt)
  (put 'project '(polynominal) project)
  (put '=zero? '(polynominal) poly-zero?)
  (put 'equ? '(polynominal polynominal) poly-equ?)
  (put 'to-string '(polynominal) to-string-poly)

  ; expose method
  (put 'extract 'polynominal-package extract-all)

  (put 'test 'polynominal-package test)
  'done)
