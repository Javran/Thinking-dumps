(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define t
  (cons
    '*table* 
    '((a . 1) (b . 2) (c . 3))))

(define (lookup key table)
  (let ((record (my-assoc key (cdr table))))
    (if record
      (cdr record)
      false)))

(define (my-assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (my-assoc key (cdr records)))))

(out
  ; lookup elements corresponding to
  ;   'a, 'b, 'c, 'd in t
  (map
    ((curry2 (flip lookup)) t)
    '(a b c d)))
; (1 2 3 #f)

(end-script)
