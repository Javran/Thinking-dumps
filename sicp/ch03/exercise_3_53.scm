(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define s
  (cons-stream
    1
    (add-streams s s)))

; 1-st element:
; s = [1,...]

; 2-nd element:
; s =   [1,...]
; s =   [1,...]
; + -----------
; s = [1,2,...]

; 3-rd element:
; s =   [1,2,...]
; s =   [1,2,...]
; + -----------
; s = [1,2,4,...]

; if n != 0:
;   (stream-ref n s) === (stream-ref (- n 1) s)
; else:
;   (stream-ref n s) = 1

; f 0 = 1
; f n = 2 * f (n-1)
; f(n) = 2^n
; s = map (2^) [0..]

(print-few 10 s)

(end-script)
