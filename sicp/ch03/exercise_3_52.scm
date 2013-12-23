(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

(define (outsum)
  (format #t "sum=~A~%" sum))

(define seq
  ; WTF to mess thing up by calling something
  ;   that has a side effect?
  (stream-map accum
              (list-in-range-stream 1 20)))
; the first one evaluted here
; 0+1 => 1, seq = [1,...]
(outsum)
; 1

(define y (stream-filter even? seq))
; keep evaluating until find one
; 1+2 => 3, seq = [1,3,...]
; 3+3 => 6, seq = [1,3,6,...]
(outsum)
; 6

(define z
  (stream-filter
    (lambda (x) (= (remainder x 5) 0))
    seq))
; 6+4 => 10, seq = [1,3,6,10,...]
(outsum)
; 10

; y = [6,...]
(out (stream-ref y 7))
; 136
; y = [6,10,...]
; y = [6,10,28,...]
; y = [6,10,28,36,...]
; y = [6,10,28,36,66,...]
; y = [6,10,28,36,66,78,...]
; y = [6,10,28,36,66,78,120,...]
; y = [6,10,28,36,66,78,120,136]
(outsum)
; 136

(display-stream z)
; requires fully evaluation of `seq`
; seq =
;   [  1,  3,  6, 10, 15
;   , 21, 28, 36, 45, 55
;   , 66, 78, 91,105,120
;   ,136,153,171,190,210
;   ]
; z = [10,15,45,55,105,120,190,210]

; outputs a `210`
(outsum)
; 210

; the responses will differ if we don't memoize the function.
; for example, `(stream-ref y 7)` will cause the first result `6` to be
; re-evaluated and trigger `accum` one more time if the result is not
; cached somewhere.

(end-script)
