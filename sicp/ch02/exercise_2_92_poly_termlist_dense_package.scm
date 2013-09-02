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
                  (adjoin-term (add term (first-term termlist))
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

  (define termlist-equ?
    ((get 'termlist-equ?-maker 'poly-generic)
     first-term
     rest-terms
     empty-termlist?))

  (define add-terms
    ((get 'add-terms-maker 'poly-generic)
     first-term
     rest-terms
     empty-termlist?
     adjoin-term))

  (define mul-term-by-all-terms
    ((get 'mul-term-by-all-terms-maker 'poly-generic)
     first-term
     rest-terms
     empty-termlist?
     make-empty
     adjoin-term))

  (define mul-terms
    ((get 'mul-terms-maker 'poly-generic)
      first-term
      rest-terms
      empty-termlist?
      make-empty
      add-terms
      mul-term-by-all-terms))

  (define neg-terms
    ((get 'neg-terms-maker 'poly-generic)
     mul-term-by-all-terms))

  (define sub-terms
    ((get 'sub-terms-maker 'poly-generic)
     add-terms
     neg-terms))

  (define div-terms
    ((get 'div-terms-maker 'poly-generic)
     first-term
     rest-terms
     empty-termlist?
     make-empty
     sub-terms
     mul-term-by-all-terms
     adjoin-term))

  (define (test)
    ((get 'test-poly-termlist 'poly-generic)
     'poly-termlist-dense
     make-empty
     make-from-args
     first-term
     rest-terms
     adjoin-term
     add-terms
     sub-terms
     mul-term-by-all-terms
     mul-terms
     div-terms
     empty-termlist?
     termlist-equ?))

  (put 'make 'poly-termlist-dense (tagged 'poly-termlist-dense make-empty))
  (put 'make-from-args 'poly-termlist-dense (tagged 'poly-termlist-dense make-from-args))
  (put 'first-term '(poly-termlist-dense) first-term)
  (put 'rest-terms '(poly-termlist-dense) (tagged 'poly-termlist-dense rest-terms))
  (put 'add '(poly-termlist-dense poly-termlist-dense) (tagged 'poly-termlist-dense add-terms))
  (put 'sub '(poly-termlist-dense poly-termlist-dense) (tagged 'poly-termlist-dense sub-terms))
  (put 'mul '(poly-termlist-dense poly-termlist-dense) (tagged 'poly-termlist-dense mul-terms))
  (put 'div '(poly-termlist-sparse poly-termlist-sparse)
       (lambda (l1 l2)
         (map ((curry2 attach-tag) 'poly-termlist) (div-terms l1 l2))))
  (put 'empty? '(poly-termlist-dense) empty-termlist?)
  (put '=zero? '(poly-termlist-dense) empty-termlist?)
  (put 'equ? '(poly-termlist-dense poly-termlist-dense) termlist-equ?)
  (put 'test 'poly-termlist-dense-package test)

  'done)
