(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; the suffix "-p" means pseudo code,
;; implemented but don't have a way to test it so far.

(define (an-integer-between a b)
  (if (<= a b)
      (amb a (an-integer-between (add1 a) b))
      (amb)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
