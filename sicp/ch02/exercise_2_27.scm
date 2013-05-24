(load "../common/utils.scm")

(define (my-reverse ls)
  (if (null? ls)
    ls
    (let ((rest-ls (my-reverse (cdr ls))))
      (append rest-ls (list (car ls))))))

(define (my-deep-reverse ls)
  (if (list? ls)
    ; apply recursively on each elements
    ;   then reverse the list itself
    (my-reverse (map my-deep-reverse ls))
    ; else
    ls))

(define x (list (list 1 2) (list 3 4)))

(out x
     ; ((1 2) (3 4))
     (my-reverse x)
     ; ((3 4) (1 2))
     (my-deep-reverse x)
     ; ((4 3) (2 1))
     )

(end-script)
