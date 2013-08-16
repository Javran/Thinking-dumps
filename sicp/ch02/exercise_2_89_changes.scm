(define (install-poly-termlist-dense-package)
  ; for dense poly representation,
  ;   we define the underlying list should have at least a non-zero element 
  ;   and the first element (i.e. one that has the highest order) cannot be zero
  ; so the zero for this representation is nil
  (define (make-empty) nil)
  (define the-empty-termlist make-empty)
  (define empty-termlist? null?)

  (define (first-term-order ls) (- (length ls) 1))
  (define (first-term ls)
    ; to obtain a term, we should combine the coeff with its order
    (let ((term-order (first-term-order ls))
          (term-coeff (car ls)))
      ((get 'make 'poly-term) term-order term-coeff)))
  (define rest-terms cdr)

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

  (define (test-merge-term)
    (let* ((make-term (get 'make 'poly-term))
           (gen-empty-list (lambda (len) (map (const (make-scheme-number 0))
                                              (list-in-range 1 len))))
           (testcases
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
               ))

           (list-equ? (lambda (a b)
                        (and (= (length a) (length b))
                             (apply boolean/and (map equ? a b)))))
           )
      (do-test-q merge-term testcases list-equ?)))

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

  (define (test-add-terms)
    (let* ((make-term (get 'make 'poly-term))
           (gen-empty-list (lambda (len) (map (const (make-scheme-number 0))
                                              (list-in-range 1 len))))
           (to-termlist ((curry2 map) make-scheme-number))

           (testcases
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
               ))
           (list-equ? (lambda (a b)
                        (and (= (length a) (length b))
                             (apply boolean/and (map equ? a b)))))

           )
      (do-test-q add-terms testcases list-equ?))

    )
                          

  (define (test)
    (test-merge-term)
    (test-add-terms)

    )


  (put 'make 'poly-termlist-dense (tagged 'poly-termlist-dense make-empty))
  (put 'first-term '(poly-termlist-dense) first-term)
  (put 'rest-terms '(poly-termlist-dense) (tagged 'poly-termlist-dense rest-terms))
  (put 'empty? '(poly-termlist-dense) empty-termlist?)
  (put 'test 'poly-termlist-dense-package test)

  'done)

(define (install-poly-term-package-mul-extension)

  ; note it make sense to multiple two terms and produce a new one,
  ;   which would be useful for our mul-terms

  (define order (get 'order '(poly-term)))
  (define coeff (get 'coeff '(poly-term)))
  (define make (get 'make 'poly-term))

  (define (mul-term t1 t2)
    (out t1 t2)
    (make (+   (order t1) (order t2))
          (mul (coeff t1) (coeff t2))))

  (put 'mul '(poly-term poly-term) mul-term)

  )

(install-poly-term-package-mul-extension)
(install-poly-termlist-dense-package)
