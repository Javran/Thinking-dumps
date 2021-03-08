(load "accumulate.scm")

(use-modules (srfi srfi-1)
             (srfi srfi-13)
             (srfi srfi-64))

(define (square n)
  (* n n))

(test-begin "accumulate")

; (test-skip "empty list")
(test-equal "empty list"
  (accumulate identity '()) '())

(test-equal "identity"
  (accumulate identity '(1 2 3))
  '(1 2 3))

(test-equal "1+"
  (accumulate 1+ '(1 2 3))
  '(2 3 4))

(test-equal "squares"
  (accumulate square '(1 2 3))
  '(1 4 9))

(test-equal "upcases"
  (accumulate string-upcase '("hello" "world"))
  '("HELLO" "WORLD"))

(test-equal "reverse strings"
  (accumulate string-reverse '("the" "quick" "brown" "fox" "jumps" "over" "the" "lazy" "dog"))
  '("eht" "kciuq" "nworb" "xof" "spmuj" "revo" "eht" "yzal" "god"))

(test-equal "length"
  (accumulate length
              '((a b c) (((d))) (e (f (g (h))))))
  '(3 1 2))

(test-equal "accumulate w/in accumulate"
  (accumulate (lambda (x)
                (string-join
                 (accumulate (lambda (y)
                               (string-append x y))
                             '("1" "2" "3"))
                 " "))
              '("a" "b" "c"))
  '("a1 a2 a3" "b1 b2 b3" "c1 c2 c3"))

(test-end "accumulate")
