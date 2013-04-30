(load "../common/utils.scm")

; question: can we simply write:
; (define (expmod base exp m)
;   (remainder (fast-expt base exp) m))
; to do the fast prime test?

; the answer is: no
;
; the difference is: 
; `expmod` will make sure the result is
;     within a limited range (i.e. between 0 and (m-1))
; `fast-expt` will get any x^y full calculated,
;     so `fast-expt` would have to deal with big integers 
;     and thus result in poor performance
