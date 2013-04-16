(load "../common/utils.scm")

(define (p) (p))

(define (test x y)
  (if (= x 0)
    0
    y))

(test 0 (p))

; <del>
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
; </del>

; I've made some mistake here,
; seems normal-order evaluation can terminate while applicative-order might not
; Some related discussion can be found at:
; scheme - SICP Exercise 1.5
; http://stackoverflow.com/questions/11334611/sicp-exercise-1-5
; Bill the Lizard SICP Exercises 1.1 - 1.5
; http://www.billthelizard.com/2009/10/sicp-exercises-11-15.html
; Ken Dyck's Weblog » Blog Archive » Solution to SICP Exercise 1.5
; http://www.kendyck.com/archives/2005/03/13/solution-to-sicp-exercise-15/

; I'd like to see what's wrong with my understanding, waiting answers here:
; http://stackoverflow.com/questions/16036139/seek-for-some-explanation-on-sicp-exercise-1-5
