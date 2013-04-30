(load "../common/utils.scm")

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base
                        (expmod base (- exp 1) m))
                     m))))

; let's analyze the time consumption
;
; if n is odd:
; f(n) = f(n-1) + c1,     c1 is a constant
; if n is even:
; f(n) = 2 * f(n/2) + c0, c0 is a constant
;
; so f(n) is a linear function of n ...
; on the other hand, why the original `expmod` is fast is because that 
;     it saves some internal results for later use and that
;     it suppresses the scale of `exp`
; but the procedure above fails to reuse the result of (expmod base (/ exp 2) m)
