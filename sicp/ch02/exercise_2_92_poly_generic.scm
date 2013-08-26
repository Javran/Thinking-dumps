; notice that once we have `first-term` `rest-terms` `empty?`
; we can access the termlist without knowing the underlying type.
; moreover, if we have `adjoin-term`, then `add-terms` becomes possible
; respect this idea, I'll collect some procedure makers here.
; so that when the required methods can be founded, we can make handlers
; with little pain by simply call the procedure to make one for us.
(define (install-poly-generic-package)

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

  (put 'mul-terms-maker 'poly-generic mul-terms-maker)
  (put 'neg-terms-maker 'poly-generic neg-terms-maker)
  (put 'sub-terms-maker 'poly-generic sub-terms-maker)

  'done)
