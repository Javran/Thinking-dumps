(load "../common/utils.scm")

(define (sum-it term a next b)
  (define (iter a result)
    ; let's follow the practice of 
    ;     "defining a constant throughout the iteractive process"
    ; the constant is "sum(term, a, next, b) + result
    (if (> a b)
      result ; if we find a > b, then sum should be 0, and result is `result`
      (iter (next a) ; next a is produced by applying a to `next`
            (+ (term a) ; apply a to `term` and accumulate the result to `result`
               result))))
  (iter a 0))

; verification
(out (sum-it identity 1 inc 100))
; 5050

(out (sum-it cube 1 inc 20))
; 44100
