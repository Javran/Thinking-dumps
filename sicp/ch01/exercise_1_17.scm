(load "../common/utils.scm")

(define (my-mul a b)
  (if (= b 0)
    0
    (+ a (my-mul a (- b 1)))))

; correctness verification
(out (* 123 456))
(out (my-mul 123 456))

; functions that we can utilize
(define (double x) (+ x x))
(define (halve x)
  (if (odd? x)
    (error "x is odd")
    (/ x 2)))

; the performan relies heavily on the size of 'b'
; we need to focus on reducing 'b' rather than reducing 'a'
(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((odd? b) (+ a (fast-mul a (- b 1))))
        (else (double (fast-mul a (halve b))))))

; the previous code is mistakenly written as:
; (define (fast-mul a b)
;   (cond ((= b 0) 0)
;         ((odd? a) (+ a (fast-mul a (- b 1))))
;         (else (double (fast-mul (halve a) b)))))
; which failed to reduce 'b' thus does not help on reducing time complexity


(out (fast-mul 123 456))

(time-test my-mul 1234 34567)
(time-test fast-mul 1234 34567)
; the latter should be much more faster.

; here I have an idea: swap 'a' and 'b' if 'b' is larger than 'a' ...
