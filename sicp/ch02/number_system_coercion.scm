(define (higher-type? a b)
  (define type-tower
    '(complex scheme-number rational))
  ; find index of a symbol
  (define (find-index x ls)
    (let ((tail (member x ls)))
      (if tail
        (- (length ls) (length tail))
        #f)))
  ; comparison
  (< (find-index a type-tower)
     (find-index b type-tower)))

; raise data to a higher type (curried)
(define (raise-to type)
  (lambda (data)
    (let ((data-type (type-tag data)))
      (cond ((equal? type data-type) data)
            ((higher-type? type data-type)
             ((raise-to type) (raise data)))
            (else (error "no way to raise downwards: RAISE-TO"))))))

; pick up the highest type in the list
(define (highest-type ls)
  (fold-left
    (lambda (cur-highest cur-type)
      (if (higher-type? cur-type cur-highest)
        cur-type
        cur-highest))
    (car ls)
    (cdr ls)))


(define (install-coercion-test)
  (define (test)
    ; test higher-type?
    (let ((testcases
            (list (mat 'complex 'scheme-number #t)
                  (mat 'scheme-number 'complex #f)
                  (mat 'rational 'scheme-number #f)
                  (mat 'complex 'rational #t)
                  (mat 'scheme-number 'scheme-number #f))))
      (do-test-q higher-type? testcases))
    ; test higher-type?
    (let ((testcases
            (list (mat 'scheme-number 'complex 'rational
                       'complex)
                  (mat 'rational 'rational 'scheme-number
                       'scheme-number)
                  (mat 'rational 'rational
                       'rational)
                  (mat 'scheme-number
                       'scheme-number)))
          (f (lambda args (highest-type args))))
      (do-test-q f testcases))


    )
  (put 'test 'coercion-system test)
  )
