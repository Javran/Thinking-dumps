(define neg ((curry2 apply-generic) 'neg))

; negation can be done by simply multiple a poly with -1
(let* ((neg-one (make-termlist-from-args
                 0 (make-scheme-number -1)))
       (neg-termlist
         (lambda (tl)
           (contents 
             ; invoke mul-termlist, and remove tag
             ((get 'mul '(poly-termlist poly-termlist)) tl (contents neg-one)))))
       (sub-termlist
         (lambda (a b)
           ; perform negation on b and call add-termlist
           ((get 'add '(poly-termlist poly-termlist)) a (neg-termlist b))))

       ; fetch poly accessors
       (variable (get 'variable '(polynominal)))
       (term-list (get 'term-list '(polynominal)))
       (sub-poly
         (lambda (p1 p2)
           (assert (same-variable?
                     (variable p1)
                     (variable p2))
                   "variables should be all the same")
           (make-poly
             (variable p1)
             (sub (term-list p1) (term-list p2)))))
       )
  (put 'neg '(poly-termlist) (tagged 'poly-termlist neg-termlist))
  (put 'sub '(poly-termlist poly-termlist) sub-termlist)
  (put 'sub '(polynominal polynominal) sub-poly)

  'done
  )
