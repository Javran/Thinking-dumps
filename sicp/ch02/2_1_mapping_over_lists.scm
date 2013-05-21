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

(out (map +
          (list 1 2 3)
          (list 40 50 60)
          (list 700 800 900)))
; (741 852 963)

(out (list (+ 1 40 700)
           (+ 2 50 800)
           (+ 3 60 900)))
; (741 852 963)

(out (map (lambda (x y) (+ x (* 2 y)))
          (list 1 2 3)
          (list 4 5 6))
     (map +
          (list 1 2 3)
          (list 8 10 12)))
; (9 12 15)

(end-script)
