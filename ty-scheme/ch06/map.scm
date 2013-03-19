(define out
  (lambda (what)
    (begin
      (display what)
      (newline))))

(newline)

(define add2
  (lambda (n)
    (+ n 2)))

(define test-list '(1 2 4 5))

(out test-list)

(out (map add2 test-list))
; '(3 4 6 7)

; for-each is for those functions that have side effects
(for-each out test-list)

; map and for-each can work with function that has more than one argument
(out (map cons '(1 2 3) test-list))
; zip '(1 2 3) '(1 2 4 5)
; ((1 . 1) (2 . 2) (3 . 4))

(let ((op (lambda (x y) (list x y))))
  (out (map op '(1 2 3) test-list)))
; ((1 1) (2 2) (3 4))

(out (map + '(1 2 3) test-list))
; zipWith (+) [1, 2, 3] [1, 2, 4, 5] -- haskell syntax
; (2 4 7)
