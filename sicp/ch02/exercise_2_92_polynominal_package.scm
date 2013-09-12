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
    (lambda (p1 p2)
      (if (compatible-variable?  (variable p1) (variable p2))
        (f p1 p2)
        ; p1 and p2 are not compatible,
        ; in this case, neither p1 nor p2 are not wildcards
        ; we should determine which one should be put inside
        (error "Polys not in same var"))))

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

  (define (extract-term target-var term t-var)
    (define (is-poly? data)
      (eq? 'polynominal (type-tag data)))

    ; binding: 
    ; term variable: t-var
    ; term coeff   : t-coeff
    ; term order   : t-order
    (let ((t-coeff (coeff term))
          (t-order (order term)))
      (if (is-poly? t-coeff)
        ; need further extraction
        (if (same-variable? target-var t-var)
          (let ((result (extract-poly target-var t-coeff))
                (order-poly (tagged-make-poly
                              target-var
                              (make-tl-from-args
                                'poly-termlist-sparse
                                t-order (make-scheme-number 1)))))
            ; add up t-order into the poly
            (mul result order-poly))
          ; else
          (let* ((result (extract-poly target-var t-coeff))
                 ; wrapped term
                 (wrapped (tagged-make-poly
                            t-var
                            (make-tl-from-args
                              'poly-termlist-sparse
                              t-order (make-scheme-number 1))))
                 (order-poly (tagged-make-poly
                               target-var
                               (make-tl-from-args
                                 'poly-termlist-sparse
                                 0 wrapped))))
            ; add up t-order into the poly
            (mul result order-poly))
          )
        ; else
        ; coeff is a number
        ; make term wrapped in a polynominal
        (let ((wrapped (tagged-make-poly
                         t-var
                         (make-tl-from-args
                           'poly-termlist-sparse
                           t-order t-coeff))))
          (if (same-variable? target-var t-var)
            wrapped
            ; not same
            (tagged-make-poly
              target-var
              (make-tl-from-args
                'poly-termlist-sparse
                0 wrapped)))))))

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
        (make-poly
          target-var
          (make-tl-empty 'poly-termlist-sparse))
        result)))

  (define (project x)
    (let ((tl (term-list x)))
      (if (empty? tl)
        (make-complex-ri 0 0)
        ((raise-to 'complex)
                  (coeff (first-term tl))))))

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
    ; test extract-poly
    (let* ((p1 (tagged-make-poly
                 'x
                 (make-tl-from-cseq-num
                   'poly-termlist-sparse
                   3 2 1 1))))
      (for-each
        (compose out to-string)
        (list
          p1
          (extract-term 'x (make-term-oc 2 (make-rational 2 3)) 'x)
          (extract-term 'y (make-term-oc 2 (make-rational 2 3)) 'x)
          ; (3x^3 + 2x^2 + 1x^1 + 1)x^3
          (extract-term 'x (make-term-oc 3 p1) 'x)
          )))
    ; test extract-term, same variable
    (out "====")
    (let* ((p1 (tagged-make-poly
                 'x
                 (make-tl-from-cseq-num
                   'poly-termlist-sparse
                   1 1 1)))
           ; ((x^2+x+1)x^2+(x^2+x+1)x+(x^2+x+1))x^2
           (p2 (tagged-make-poly
                 'x
                 (make-tl-from-args
                   'poly-termlist-sparse
                   2 p1
                   1 p1
                   0 p1))))
      (out (to-string
             (tagged-make-poly 
               'x
               (make-tl-from-args
                 'poly-termlist-sparse
                 2 p2))))
      (out (to-string (extract-term 'x (make-term-oc 2 p2) 'x))))
    (out "====")
    ; (x^2+2x+3)y^2
    (let* ((p1 (tagged-make-poly
                 'x
                 (make-tl-from-cseq-num
                   'poly-termlist-sparse
                   1 2 3)))
           )
      (out (to-string
             (extract-term 'x (make-term-oc 2 p1) 'y))))

    (out "====")
    ; (x^2+x+1)*(y^2+y+1)*z^2
;     (let* ((p1 (tagged-make-poly
;                  'x
;                  (make-tl-from-cseq-num
;                    'poly-termlist-sparse
;                    1 1 1)))
;            (p2 (tagged-make-poly
;                  'y
;                  (make-tl-from-cseq-num
;                    'poly-termlist-sparse
;                    1 1 1)))
;            (p3 (mul p2
;                     (tagged-make-poly
;                       'y
;                       (make-tl-from-args
;                         'poly-termlist-sparse
;                         0 p1)))))
;       (out (to-string (extract-term 'x (make-term-oc 2 p3) 'z)))
;       )



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

  (put 'test 'polynominal-package test)
  'done)
