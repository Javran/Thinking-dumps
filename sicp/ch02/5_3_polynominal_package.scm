(define (install-polynomial-package)
  ; data structure tag: poly
  ; each "poly" is consisted by a single variable & term list
  ; accessors:
  ;   * variable
  ;   * term-list
  ; for variables:
  ;   * same-variable?: precondition of `add-poly` and `mul-poly`
  ; for terms:
  ;   * the-empty-term-list
  ;   * add-terms
  ;   * mul-terms
  ;   * adjoin-term: add new term into the list
  ;   * first term | rest terms
  ;   * order & coeff
  ; for poly:
  ;   * add-poly
  ;   * mul-poly
  ; example:
  ;  2*x^3+4*x+1
  ;   * terms:
  ;     2*x^3(coeff=2,order=3),
  ;     4*x(coeff=4,order=1),
  ;     1(coeff=1,order=0)
  ;   * variable: x

  (define make-poly cons)
  (define variable car)
  (define term-list cdr)

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add (term-list p1) (term-list p2)))
      (error "Polys not in same var: ADD-POLY"
             (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul (term-list p1) (term-list p2)))
      (error "Polys not in same var: MUL-POLY"
             (list p1 p2))))

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

  (define =zero?
    ; TODO: placeholder
    (const #f))

  (put 'add '(polynominal polynominal) (tagged 'polynominal add-poly))
  (put 'mul '(polynominal polynominal) (tagged 'polynominal mul-poly))
  (put 'make 'polynominal (tagged 'polynominal make-poly))
  (put 'variable '(polynominal) variable)
  (put '=zero? '(polynominal) =zero?)
  (put 'to-string '(polynominal) to-string)
  (put 'term-list '(polynominal) term-list)
  'done)
