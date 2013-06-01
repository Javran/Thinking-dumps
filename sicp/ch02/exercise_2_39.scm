(load "../common/utils.scm")

(define (reverse-1 seq)
  (fold-right (lambda (i acc) (append acc (list i))) nil seq))

(define (reverse-2 seq)
  (fold-left (lambda (acc i) (cons i acc)) nil seq))

(let ((ls (list-in-range 1 10)))
  (out (reverse-1 ls))
  (out (reverse-2 ls)))

(end-script)
