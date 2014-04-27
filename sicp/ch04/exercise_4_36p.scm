(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; simply changing `an-integer-between` to
;; `an-integer-starting-from` will not work
;; because `try-again` will spend infinite time
;; searching through the last integer (i.e. `k`
;; in the previous exercise) while keep other group
;; untouched.

;; when we were dealing with streams in chapter 3,
;; we can "merge" two streams together to allow both
;; stream to get a chance of being used.
;; but in `amb`, we don't have much control on
;; the searching / iterating strategy.
;; So I think constraining the search space using
;; mathematical approach might be the easiest way
;; for this problem:
;;
;; first consider the constraint:
;;   i * i + j * j = k * k
;; because this relation is equality,
;; so we actually don't need to search through `k`
;;
;; now we only search through i and j, since i <= j
;; we can first grow `j` and let `i` be an integer between
;; `low` and `j`.
;;
;; by using the strategy above we are able to traverse all
;; the possible solutions.

(define (a-pythagorean-triple-starting-from low)
  (let ((j (an-integer-starting-from low)))
    (let ((i (an-integer-between low j)))
      (let ((maybe-k (sqrt (* i i) (* j j))))
        (require (integer? maybe-k))
        (list i j k)))))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
