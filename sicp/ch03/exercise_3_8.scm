(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define f
  ; keep a counter
  ;   which increases by 1 each time when called
  ;   and this procedure only cares about the call with
  ;   counter being odd numbers. (the side effect is caused by the counter
  ; 
  (let ((last #f)
        (counter 0))
    (define (side-effect x)
      (set! counter (inc counter))
      (if (odd? counter)
        ; counter is odd
        ;   -> set last
        (begin
          (set! last x)
          x)
        ; counter is even
        ;   -> simply return 0
        0))
    side-effect))

(let ((a (f 0)))
  ; force it
  a
  (let ((b (f 1)))
    (out (+ a b))))
; 0

(let ((b (f 1)))
  ; force it
  b
  (let ((a (f 0)))
    (out (+ a b))))
; 1

(end-script)
