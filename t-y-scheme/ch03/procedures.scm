(define out
  (lambda (what)
    (begin
      (display what)
      (newline))))

(newline)

(out (lambda (x) (+ x 2)))
; procedure
(out ((lambda (x) (+ x 2)) 5))
; 7

(define add2
  (lambda (x) (+ x 2)))

(out (add2 4))
; 6

(out (add2 9))
; 11

(define area-1
  (lambda (length breadth)
    (* length breadth)))

(define area-2 *)

(out (area-1 10 20))
(out (area-2 10 20))
; 200

; test variable arguments
(define test-arg
  (lambda (a b . c)
    (begin
      (out a)
      (out b)
      (out c))))

(test-arg 1 2 3)
; 1 2 (3)
(test-arg 1 2 3 4 5)
; 1 2 (3 4 5)
