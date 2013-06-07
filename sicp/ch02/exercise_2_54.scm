(load "../common/utils.scm")

(define (my-equal? a b)
  (cond ((and (symbol? a)
              (symbol? b))
          (eq? a b))
        ((and (list? a)
              (list? b))
          (cond ((and (null? a)
                      (null? b))
                  #t)
                ((or  (null? a)
                      (null? b))
                 #f)
                (else (and (my-equal? (car a) (car b))
                           (my-equal? (cdr a) (cdr b))))))
        (else #f)))

(out
  (my-equal? '(this is a list) '(this is a list))
  ; #t
  (my-equal? '(this is a list) '(this (is a) list))
  ; #f
  )

(end-script)
