(load "../common/utils.scm")

; (define (<name> <formal parameters>) <body>)
(define (square x) (* x x))

(out
  (square 21)
  ; 441
  (square (+ 2 5))
  ; 49
  (square (square 3))
  ; 81
  )

; we can now use 'square' as building blocks of other procdures

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(out (sum-of-squares 3 4))

; furthermore ...
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(out (f 5))
; 136
; (+ 5 1) -> 6, (* a 2) -> 10
