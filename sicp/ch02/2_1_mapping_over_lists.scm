(load "../common/utils.scm")

(define (scale-list items factor)
  (if (null? items)
    nil
    (cons (* (car items) factor)
          (scale-list (cdr items) factor))))

(out (scale-list (list-in-range 1 10) 10))
; (10 20 30 ... 100)

(define (my-map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
          (my-map proc (cdr items)))))

(out (my-map abs (list -10 2.5 -11.6 17)))
; (10 2.5 11.6 17)

(out (my-map square (list 1 2 3 4)))
; (1 4 9 16)

(define (new-scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(out (new-scale-list (list-in-range 1 10) 10))
; (10 20 30 ... 100)

(end-script)
