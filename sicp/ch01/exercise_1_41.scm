(load "../common/utils.scm")

(define (double f)
  (lambda (x)
    (f (f x))))

(out
  (inc 1)
  ; 2
  ((double inc) 1)
  ; 3
  ((double (double inc)) 1))
  ; 5

; (error "comment this line for spoiler")

(out (((double (double double)) inc) 5))
; this exercise is really a brain fucker and makes no sense in practice ...
; to expand everything is almost impossible and error-prone ... let's do it by analysis
; (double double) => apply f 2^2 times
; (double (double double)) => apply f 2^(2^2) times => 16 times

; the output is 5 + 2^(2^2)
