(load "strain.scm")

(define (under-10? n)
  (< n 10))

(define (starts-with-z? s)
  (char=? (string-ref s 0) #\z))

(use-modules (srfi srfi-64))

(test-begin "strain")

; (test-skip "empty keep")
(test-equal "empty keep"
  (keep under-10? '())
  '())

(test-equal "keep everything"
  (keep under-10? '(0 2 4 6 8))
  '(0 2 4 6 8))

(test-equal "keep first last"
  (keep odd? '(1 2 3))
  '(1 3))

(test-equal "keep nothing"
  (keep even? '(1 3 5 7 9))
  '())

(test-equal "keep neither first nor last"
  (keep even? '(1 2 3))
  '(2))

(test-equal "keep strings"
  (keep starts-with-z? '("apple" "zebra" "banana" "zombies" "cherimoya" "zealot"))
  '("zebra" "zombies" "zealot"))

(test-equal "empty discard"
  (discard under-10? '())
  '())

(test-equal "discard everything"
  (discard under-10? '(1 2 3))
  '())

(test-equal "discard first and last"
  (discard odd? '(1 2 3))
  '(2))

(test-equal "discard nothing"
  (discard even? '(1 3 5 7 9))
  '(1 3 5 7 9))

(test-equal "discard neither first nor last"
  (discard even? '(1 2 3))
  '(1 3))

(test-equal "discard strings"
  (discard starts-with-z? '("apple" "zebra" "banana" "zombies" "cherimoya" "zealot"))
  '("apple" "banana" "cherimoya"))

(test-end "strain")
