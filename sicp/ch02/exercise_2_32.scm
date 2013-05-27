(load "../common/utils.scm")

(define (subsets s)
  (if (null? s)
    ; if `s` is an empty list, nothing except an empty list is contained
    ;     in the result
    (list nil)
    ; else
    (let ((rest (subsets (cdr s))))
      (append rest
              (map (lambda (x) (cons (car s) x))
                   rest)))))

(out (subsets (list 1 2 3)))


(end-script)
