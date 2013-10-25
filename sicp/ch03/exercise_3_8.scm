(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define g
  ; keep a counter
  ;   which increases by 1 each time when called
  ;   and this procedure only cares about the call with
  ;   counter being odd numbers.
  (let ((last #f)
        (counter 0))
    (define (side-effect x)
      (set! counter (inc counter))
      (if (odd? counter)
        (begin
          (set! last x)
          x)
        last))
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
; 2

(end-script)
