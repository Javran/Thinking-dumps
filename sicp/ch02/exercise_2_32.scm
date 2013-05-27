(load "../common/utils.scm")

(define (subsets s)
  (if (null? s)
    ; if `s` is an empty list, nothing except an empty list is contained
    ;     in the result
    (list nil)
    ; else
    (let ((rest (subsets (cdr s))))
      ; two situations are taken into account
      ;     very similar to what we've done in example "counting change"
      (append 
          ; we ignore the first element and calculate the subset of rest elements
          rest
          ; and now we have to consider the first element:
          ;     simply `cons` the first element into every list contains in the list `rest`
          (map (lambda (x) (cons (car s) x))
                   rest)))))

(out (subsets (list 1 2 3)))

(end-script)
