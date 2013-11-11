(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define t
  (cons
    '*table* 
    '((a . 1) (b . 2) (c . 3))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      false)))

(out
  (map
    ((curry2 (flip lookup)) t)
    '(a b c d)))
; (1 2 3 #f)

(end-script)
