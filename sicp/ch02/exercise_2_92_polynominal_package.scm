(define (install-polynomial-package)
  (define make-poly cons)
  (define variable car)
  (define term-list cdr)

  ; verify variable equality before performing a binary op
  (define (variable-verify f)
    (lambda (p1 p2)
      (assert (same-variable? (variable p1)
                              (variable p2))
              "Polys not in same var")
      (f p1 p2)))

  ; operations without variable verification
  (define (add-poly1 p1 p2)
    (make-poly (variable p1)
               (add (term-list p1) (term-list p2))))

  (define (mul-poly1 p1 p2)
    (make-poly (variable p1)
               (mul (term-list p1) (term-list p2))))

  (define (sub-poly1 p1 p2)
    (make-poly (variable p1)
               (sub (term-list p1) (term-list p2))))

  ; operations with verification
  (define add-poly (variable-verify add-poly1))
  (define mul-poly (variable-verify mul-poly1))
  (define sub-poly (variable-verify sub-poly1))

  (define (to-string p)
    (define (generic-to-string x)
      (apply-generic 'to-string x))
    (define (term-to-string term var)
      (string-append 
        "("
        (generic-to-string (coeff term))
        ")"
        (symbol->string var)
        "^"
        (number->string (order term))))
    (define (first-term x)
      (apply-generic 'first-term x))
    (define (rest-terms x)
      (apply-generic 'rest-terms x))
    (define (empty-termlist? x)
      (apply-generic 'empty? x))
    (let ((var (variable p))
          (termls (term-list p)))
      (cond ((empty-termlist? termls) "0")
            ((empty-termlist? (rest-terms termls))
              ; only one element
              (term-to-string (first-term termls) var))
            (else
              (string-append
                (term-to-string (first-term termls) var)
                "+"
                (to-string (make-poly var (rest-terms termls))))))))

  (define poly-zero?
    (compose =zero? term-list))

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
    )

  (put 'make 'polynominal (tagged 'polynominal make-poly))

  (put 'variable '(polynominal) variable)
  (put 'term-list '(polynominal) term-list)

  (put 'add '(polynominal polynominal) (tagged 'polynominal add-poly))
  (put 'mul '(polynominal polynominal) (tagged 'polynominal mul-poly))
  (put 'sub '(polynominal polynominal) (tagged 'polynominal sub-poly))
  (put '=zero? '(polynominal) poly-zero?)
  (put 'to-string '(polynominal) to-string)

  (put 'test 'polynominal-package test)
  'done)
