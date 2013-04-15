(load "../common/utils.scm")

(define (p) (p))

(define (test x y)
  (if (= x 0)
    0
    y))

(test 0 (p))

; for normal-order evaluation, we need to fully expand the expression:
; (test 0 (p)) ; x -> 0, y -> (p)
; (if (= 0 0) 0 (p))
; since "(p)" evaluates to "(p)", it will run into an infinite loop

; for applicative-order evaluation, we only evaluate things as needed
; so:
; (test 0 (p)) ; x -> 0, y -> (p)
; (if (= 0 0) 0 (p))
; we can say that "(= 0 0)" evaluates to #t,
;     so "(p)" is no longer needed to be evaluated to produce the result
; so the whole expression will result in "0" and the procedure can be terminated normally
