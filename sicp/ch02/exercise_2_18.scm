(load "../common/utils.scm")

(define (my-reverse ls)
  (if (null? ls)
    ls
    (let ((rest-ls (my-reverse (cdr ls))))
      (append rest-ls (list (car ls))))))

(out (my-reverse (list-in-range 1 10)))
; (10 9 8 7 ... 1)

(out (my-reverse (map square (list-in-range 1 5))))
; (25 16 9 4 1)

(end-script)
