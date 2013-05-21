(load "../common/utils.scm")

(define (square-list-1 items)
  (if (null? items)
    nil
    (cons (square (car items))
          (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))

(out (square-list-1 (list-in-range 1 10))
     (square-list-2 (list-in-range 1 10)))

; (1 4 9 ... 100)

(end-script)
