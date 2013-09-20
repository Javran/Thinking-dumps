;(load "./5_3_polynominal_setup.scm")
;(load "./exercise_2_90_changes.scm")
;(load "./exercise_2_91_changes.scm")

(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (equal? v1 v2)))

; when (variable-less? v1 v2) returns #t,
;   that means v1 should be put outside
(define (variable-less? v1 v2)
  (string<? (symbol->string v1)
            (symbol->string v2)))

(load "./exercise_2_92_poly_term_package.scm")
(load "./exercise_2_92_poly_generic.scm")
(load "./exercise_2_92_poly_termlist_sparse_package.scm")
(load "./exercise_2_92_poly_termlist_dense_package.scm")
(load "./exercise_2_92_polynominal_package.scm")

(install-poly-term-package)
(install-poly-generic-package)
(install-poly-termlist-sparse-package)
(install-poly-termlist-dense-package)
(install-polynomial-package)

(define make-poly (get 'make 'polynominal))
(define variable ((curry2 apply-generic) 'variable))
(define term-list ((curry2 apply-generic) 'term-list))

; better remember the arg-list by its name(order-coeff)
(define make-term-oc (get 'make 'poly-term))
(define order ((curry2 apply-generic) 'order))
(define coeff ((curry2 apply-generic) 'coeff))

; make termlist(tl) from args, first arg is the type,
;   other args will be forwarded accordingly
(define (make-tl-from-args type . args)
  (apply (get 'make-from-args type) args))
; make an empty term list
(define (make-tl-empty type)
  ((get 'make type)))
; make termlist(tl) from coeff seq
;   e.g. args: 1 2 3 4 produces a termlist
;     representing 1 x^3 + 2 x^2 + 3 x + 4
(define (make-tl-from-cseq type . coeffs)
  (let ((orders (reverse! (list-in-range 0 (- (length coeffs) 1)))))
    (apply make-tl-from-args
           (cons type
                 (apply append (map list orders coeffs))))))

; cast all numbers to scheme-number automatically
(define (make-tl-from-cseq-num type . coeffs)
  (apply make-tl-from-cseq
         (cons type
               (map make-scheme-number coeffs))))

(define (to-order-coeff-list ls)
  (if (empty? ls)
    '()
    (let ((t (first-term ls)))
      (cons (order t)
            (cons (coeff t)
                  (to-order-coeff-list (rest-terms ls)))))))

(define (to-poly-termlist-type termlist type)
  (if (eq? type (type-tag termlist))
    termlist
    (apply (get 'make-from-args type)
           (to-order-coeff-list termlist))))

(define first-term ((curry2 apply-generic) 'first-term))
(define rest-terms ((curry2 apply-generic) 'rest-terms))
(define empty? ((curry2 apply-generic) 'empty?))

(define (map-poly-term f ls)
  (if (empty? ls)
    nil
    (cons (f (first-term ls))
          (map-poly-term f (rest-terms ls)))))

(define order-list ((curry2 map-poly-term) order))
(define coeff-list ((curry2 map-poly-term) coeff))

(define (is-poly? data)
  (eq? 'polynominal (type-tag data)))

(define extract
  (get 'extract 'polynominal-package))

(run-tests 
  (list 
    'poly-term-package
    'poly-termlist-sparse-package
    'poly-termlist-dense-package
    'polynominal-package
    ))
