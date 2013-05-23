(load "../common/utils.scm")

(for-each out (list 57 321 88))
; 57 321 88

(define (my-foreach f ls)
  (if (non-empty? ls)
    (begin
      (f (car ls))
      (my-foreach f (cdr ls)))))

(my-foreach out (list 57 321 88))
; 57 321 88

(end-script)
