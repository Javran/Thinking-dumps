(define (install-poly-termlist-dense-package)
  (define (make-empty) nil)
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
      (fold-right adjoin-term
                 (make-empty)
                 terms)))

  (define (first-term-order ls) (- (length ls) 1))
  (define (first-term ls)
    ; to obtain a term, we should combine the coeff with its order
    (let ((term-order (first-term-order ls))
          (term-coeff (car ls)))
      ((get 'make 'poly-term) term-order term-coeff)))
  (define (rest-terms ls)
    (drop-while =zero? (cdr ls)))

  (define (adjoin-term term termlist)
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
                  ; this part was affected by new rest-terms
                  ; TODO: try to find a better impl
                  (list-modify termlist 
                               (- ft-order t-order)
                               (add (list-ref termlist (- ft-order t-order))
                                    t-coeff))))))))))
  (define (list-modify ls ind val)
    (if (= ind 0)
      (cons val (cdr ls))
      (cons (car ls) (list-modify (cdr ls) (- ind 1) val))))

  (define (add-terms l1 l2)
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

  (put 'make 'poly-termlist-dense (tagged 'poly-termlist-dense make-empty))
  (put 'make-from-args 'poly-termlist-dense (tagged 'poly-termlist-dense make-from-args))
  (put 'first-term '(poly-termlist-dense) first-term)
  (put 'rest-terms '(poly-termlist-dense) (tagged 'poly-termlist-dense rest-terms))
  (put 'add '(poly-termlist-dense poly-termlist-dense) (tagged 'poly-termlist-dense add-terms))
;  (put 'sub '(poly-termlist-dense poly-termlist-dense) (tagged 'poly-termlist-dense sub-terms))
  (put 'mul '(poly-termlist-dense poly-termlist-dense) (tagged 'poly-termlist-dense mul-terms))
  (put 'empty? '(poly-termlist-dense) empty-termlist?)
  (put '=zero? '(poly-termlist-dense) empty-termlist?)
;  (put 'order-list '(poly-termlist-dense) order-list)
;  (put 'coeff-list '(poly-termlist-dense) coeff-list)
;  (put 'equ? '(poly-termlist-dense poly-termlist-dense) termlist-equ?)
;
;  (put 'test 'poly-termlist-dense-package test)

  'done)
