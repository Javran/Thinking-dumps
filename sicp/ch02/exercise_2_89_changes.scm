(define (install-poly-termlist-dense-package)
  ; for dense poly representation,
  ;   we define the underlying list should have at least a non-zero element 
  ;   and the first element (i.e. one that has the highest order) cannot be zero
  ; so the zero for this representation is nil
  (define (make-empty) nil)
  (define the-empty-termlist make-empty)
  (define empty-termlist? null?)

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
      (fold-right merge-term (make-empty) terms)))

  (define (first-term-order ls) (- (length ls) 1))
  (define (first-term ls)
    ; to obtain a term, we should combine the coeff with its order
    (let ((term-order (first-term-order ls))
          (term-coeff (car ls)))
      ((get 'make 'poly-term) term-order term-coeff)))
  (define (rest-terms ls)
    (drop-while =zero? (cdr ls)))

  (define coeff ((curry2 apply-generic) 'coeff))
  (define order ((curry2 apply-generic) 'order))

  ; merge a term into the list
  (define (merge-term term termlist)
    (let ((const-zero (const (make-scheme-number 0)))
          (t-coeff (coeff term))
          (t-order (order term)))
      (cond
        ; case #1: the term is zero
        ;   nothing to do
        ((=zero? t-coeff) termlist)
        ; + precond: term is non-zero
        ; case #2: the term list is empty
        ;   make a placeholder list of length {t-order}
        ;   e.g. to insert a term of order=3, coeff=x we need an empty list (0 0 0)
        ;         after that we simply put x in front of this list and produce (x 0 0 0)
        ((empty-termlist? termlist)
              (cons t-coeff
                    (map const-zero (list-in-range 1 t-order))))
        ; + precond: termlist is non-empty
        (else
          (let ((ft-order (first-term-order termlist)))
            ; case #3:
            ;   assume we are inserting a term into an empty list,
            ;   we need a place-holder list of length {t-order}
            ;   now we already have a list of length {ft-order + 1}
            ;   when t-order >= ft-order + 1, we need extra spaces for padding (including zero)
            (if (>= t-order (+ ft-order 1))
              (cons t-coeff
                    (append (map const-zero (list-in-range 1 (- t-order (+ ft-order 1))))
                            termlist))
              ; else we simple find the corresponding position and add coeff to it
              ; * note it's possible in this case 
              ;   that the rule of non-zero first term might be violated,
              ;   so we will try to remove leading zeros when the merge is done
              (drop-while
                =zero?
                (if (= t-order ft-order)
                  ; case #4: t-order = ft-order
                  (cons (add (coeff (first-term termlist))
                             t-coeff)
                        (rest-terms termlist))
                  ; case #5: t-order < ft-order
                  (cons (coeff (first-term termlist))
                        (merge-term term (rest-terms termlist)))))))))))

  (define (add-terms l1 l2)
    ; there're 2 ways of implementing add-terms
    ; 1. use first-term and rest-terms to retrieve info about the termlist
    ; 2. use structure-related code, we can simple add coeffs accordingly, 
    ;     and result in higher performance, but it would make rest-terms and first-term
    ;     placed in the same `language` level of add-terms
    ; I'll use the first method
    (cond ((empty-termlist? l1) l2)
          ((empty-termlist? l2) l1)
          (else (let ((ft1 (first-term-order l1))
                      (ft2 (first-term-order l2)))
                  (cond ((equ? ft1 ft2)
                          (merge-term ((get 'make 'poly-term)
                                        ft1
                                        (add (coeff (first-term l1))
                                             (coeff (first-term l2))))
                                      (add-terms (rest-terms l1)
                                                 (rest-terms l2))))
                        ((> ft1 ft2)
                          (merge-term (first-term l1)
                                      (add-terms (rest-terms l1)
                                                 l2)))
                        ((< ft1 ft2)
                          (merge-term (first-term l2)
                                      (add-terms l1
                                                 (rest-terms l2))))
                        (else (error "impossible case")))))))
                          
  (define (mul-term-by-all-terms t1 l)
    (if (empty-termlist? l)
      (the-empty-termlist)
      (merge-term (mul t1 (first-term l))
                  (mul-term-by-all-terms t1 (rest-terms l)))))

  (define (mul-terms l1 l2)
    (if (empty-termlist? l1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term l1) l2)
                 (mul-terms (rest-terms l1) l2))))

  (define (test)
    (let* ((make-term (get 'make 'poly-term))
           (gen-empty-list (lambda (len) (map (const (make-scheme-number 0))
                                              (list-in-range 1 len))))
           (to-termlist ((curry2 map) make-scheme-number))
           (list-equ? (lambda (a b)
                        (and (= (length a) (length b))
                             (apply boolean/and (map equ? a b)))))
           )
      ; test accessors
      (let ((testcases
              (list (mat (to-termlist (list 1 0 2 3 4))
                         ; result: (cons <first-term-result> <rest-terms-result>)
                         (cons (make-term 4 (make-scheme-number 1))
                               (to-termlist (list 2 3 4))))
                    (mat (to-termlist (list 1 0 0 0 0 0 0 0))
                         (cons (make-term 7 (make-scheme-number 1))
                               nil))
                    ))
            (f (lambda (x) (cons (first-term x) (rest-terms x))))
            (result-eq?
              (lambda (a b)
                (and (equ? (car a) (car b)) 
                     (= (length (cdr a)) (length (cdr b)))
                     (apply boolean/and (map equ? (cdr a) (cdr b))))))
            )
        (do-test-q f testcases result-eq?))
      ; test merge-term
      (let ((testcases
              (list
                ; case #1
                (mat (make-term 10 (make-scheme-number 0)) (map make-scheme-number (list 1 2 3 4))
                     (map make-scheme-number (list 1 2 3 4)))
                ; case #2
                (mat (make-term 4 (make-scheme-number 7)) nil 
                     (map make-scheme-number (list 7 0 0 0 0)))
                ; case #3
                (mat (make-term 4 (make-scheme-number 7)) (map make-scheme-number (list 1 2 3))
                     (map make-scheme-number (list 7 0 1 2 3)))
                ; case #3
                (mat (make-term 4 (make-scheme-number 7)) (map make-scheme-number (list 1 2 3 4))
                     (map make-scheme-number (list 7 1 2 3 4)))
                ; case #4
                (mat (make-term 4 (make-scheme-number 7)) (map make-scheme-number (list 1 2 3 4 5))
                     (map make-scheme-number (list 8 2 3 4 5)))
                ; case #5
                (mat (make-term 1 (make-scheme-number 5)) (map make-scheme-number (list 1 2 3 4 5))
                     (map make-scheme-number (list 1 2 3 9 5)))
                ; case #5
                (mat (make-term 0 (make-scheme-number 4)) (map make-scheme-number (list 1 2 3 4 5))
                     (map make-scheme-number (list 1 2 3 4 9)))
                ; case #5
                (mat (make-term 4 (make-scheme-number -5)) (map make-scheme-number (list 5 0 0 0 0))
                     nil)
                )))
        (do-test-q merge-term testcases list-equ?))
      ; test add-terms
      (let ((testcases
              (list
                (mat nil nil
                     nil)
                (mat nil (to-termlist (list 1 2 3 4 5))
                     (to-termlist (list 1 2 3 4 5)))
                (mat (to-termlist (list 5 4 3 2 1)) nil
                     (to-termlist (list 5 4 3 2 1)))
                (mat (to-termlist (list 3 2 1)) (to-termlist (list 5 4 0 0 0))
                     (to-termlist (list 5 4 3 2 1)))
                (mat (to-termlist (list 5 4 3 0 0)) (to-termlist (list 2 1))
                     (to-termlist (list 5 4 3 2 1)))
                (mat (to-termlist (list 5 4 3 2 1)) (to-termlist (map - (list 5 4 3 2 1)))
                     nil)
                )))
        (do-test-q add-terms testcases list-equ?))
      ; test mul-term-by-all-terms
      (let ((testcases
              (list
                (mat (make-term 2 (make-scheme-number 4)) (to-termlist (list 1 2 3))
                     (to-termlist (list 4 8 12 0 0)))
                (mat (make-term 7 (make-scheme-number 0)) (to-termlist (list 1 2 3 4 5 6))
                     nil)
                (mat (make-term 100 (make-scheme-number 100)) nil
                     nil)
                )))
        (do-test-q mul-term-by-all-terms testcases list-equ?))
      ; test mul-terms
      (let ((testcases
              (list
                (mat nil nil
                     nil)
                (mat nil (to-termlist (list 1 2 3 4 5))
                     nil)
                (mat (to-termlist (list 1 2 3 4 5)) nil
                     nil)
                (mat (to-termlist (list 1 2 3)) (to-termlist (list 4 5 6))
                     ; (x^2 + 2x + 3) * (4x^2 + 5x + 6)
                     ; => 4x^4 + 13x^3 + 28^x2 + 27x + 18
                     (to-termlist (list 4 13 28 27 18)))
                (mat (to-termlist (list 5 0 0 7 0 9 0)) (to-termlist (list 2 0 4 0 0))
                     ;(5x^6 + 7x^3 + 9x) * (2x^4 + 4x^2)
                     ; => 10x^10 + 20x^8 + 14x^7 + 46x^5 + 36x^3
                     (to-termlist (list 10 0 20 14 0 46 0 36 0 0 0)))
                )))
        (do-test-q mul-terms testcases list-equ?))
      ; test make-from-args
      (let ((testcases
              (list
                (mat 1 (make-scheme-number 3)
                     2 (make-scheme-number 2)
                     3 (make-scheme-number 1)
                     (to-termlist (list 1 2 3 0)))
                (mat 1 (make-scheme-number 2)
                     3 (make-scheme-number 4)
                     (to-termlist (list 4 0 2 0)))
                (mat 10 (make-scheme-number 3)
                     (to-termlist (cons 3 (gen-empty-list 10))))
                )))
        (do-test-q make-from-args testcases list-equ?))
      ))

  (put 'make 'poly-termlist-dense (tagged 'poly-termlist-dense make-empty))
  (put 'make-from-args 'poly-termlist-dense (tagged 'poly-termlist-dense make-from-args))
  (put 'first-term '(poly-termlist-dense) first-term)
  (put 'rest-terms '(poly-termlist-dense) (tagged 'poly-termlist-dense rest-terms))
  (put 'add '(poly-termlist-dense poly-termlist-dense) (tagged 'poly-termlist-dense add-terms))
  (put 'mul '(poly-termlist-dense poly-termlist-dense) (tagged 'poly-termlist-dense mul-terms))
  (put 'empty? '(poly-termlist-dense) empty-termlist?)
  (put 'test 'poly-termlist-dense-package test)

  'done)

(define (install-poly-term-package-extension)

  ; note it make sense to multiple two terms and produce a new one,
  ;   which would be useful for our mul-terms

  (define order (get 'order '(poly-term)))
  (define coeff (get 'coeff '(poly-term)))
  (define make (get 'make 'poly-term))

  (define (equ-term? a b)
    (and (=    (order a) (order b))
         (equ? (coeff a) (coeff b))))

  (define (mul-term t1 t2)
    (make (+   (order t1) (order t2))
          (mul (coeff t1) (coeff t2))))

  (put 'mul '(poly-term poly-term) mul-term)
  (put 'equ? '(poly-term poly-term) equ-term?)
  'done
  )

(install-poly-term-package-extension)
(install-poly-termlist-dense-package)

; insert 2 makes into global env
(define make-termlist-from-args-dense
  (get 'make-from-args 'poly-termlist-dense))
(define the-empty-term-list-dense
  (get 'make 'poly-termlist-dense))

; termlist accessors
(define first-term ((curry2 apply-generic) 'first-term))
(define rest-terms ((curry2 apply-generic) 'rest-terms))

(define term-list ((curry2 apply-generic) 'term-list))
