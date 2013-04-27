(load "../common/utils.scm")

(define (my-gcd a b)
  (if (= b 0)
    a
    (my-gcd b (remainder a b))))

(out (my-gcd 12345 65535))
; 15
