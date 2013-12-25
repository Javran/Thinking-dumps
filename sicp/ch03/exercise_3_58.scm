(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define (expand num den radix)
  ; gives a sequence of the fractional part of `num / den`
  ;   represented under base `radix` 
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

(print-few 10 (expand 1 5 10))
; 1/5 = 0.2 (base 10)
(print-few 10 (expand 1 5 2))
; 1/5 = 0.00110011.. (base 2)

; 1/7 = 0.1428571428.. (base 10)
; 3/8 = 0.3750000000.. (base 10)

(print-few 10 (expand 1 7 10))
(print-few 10 (expand 3 8 10))

(end-script)
