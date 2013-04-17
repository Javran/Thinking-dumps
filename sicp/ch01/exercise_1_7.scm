(load "../common/utils.scm")

(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

; given a guess, how to improve it?
(define (improve guess x)
  ; average of guess & (x/guess)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(define (my-sqrt x) (sqrt-iter 1.0 x))

; an example for "good-enough?" is inadequate for small numbers
(out
  (my-sqrt 0.0001)
  ; 0.03 <- might considered a big error
  (sqrt 0.0001)
  ; 0.01
  )

; an example for "good-enough?" is inadequate for big numbers
; actually I don't know why it is inadequate ...
; the point might be: it's already precise enough for big numbers,
;     but the fixed precision cannot satisfiy both small numbers and big numbers
(out
  (my-sqrt 100000)
  ; 316.2
  (sqrt 100000)
  ; 316.2
  )

; I'd like to abstract the computation of Newton's method.
; My idea is:
; TODO: the code here is just a naive skeleton, need to make it work
(define do-newton-method
  (lambda (prev-guess     ; the previous guess, #f for the initial call
           current-guess  ; the current guess
           target-func    ; target func, we want (target-func current-guess) to be zero
           refine-func    ; how we refine a value, 
                          ;     will be called like (refine-func current-guess target)
           good-enough?   ; criteria of the value, do we need to stop?
                          ;     will be called like (good-enough? prev-guess current-guess)
           )
    (if (not prev-guess)
      ; if it's the initial call
      (do-newton-method
        current-guess
        (refine-func current-guess (target-func current-guess))
        target-func
        refine-func
        good-enough?)
      ; else we have a prev-guess available
      (if (good-enough? prev-guess current-guess)
        ; yes, good-enough, return it.
        current-guess
        ; need to refine
        (do-newton-method
          current-guess 
          (refine-func current-guess (target-func current-guess))
          target-func
          refine-func
          good-enough?)))))
