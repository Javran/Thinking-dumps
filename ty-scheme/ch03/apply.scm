(load "../common/utils.scm")

(define x '(1 2 3 4))

(out (apply + x))
; 10

(out (apply * x))
; 24

(define var-arg
  (lambda (a b . c)
    (begin
      (out a)
      (out b)
      (out c))))

(apply var-arg 10 20 30 x)
(apply var-arg 10 20 30 '(1 2 3 4))
; 10, 20, (30 1 2 3 4)

(out (apply + 1 x))
; 11
