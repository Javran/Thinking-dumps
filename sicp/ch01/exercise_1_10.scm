(load "../common/utils.scm")

; (error "uncomment this line to see the result")

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; => (A 1 10)
; => (cond ((= y 0) _)
;          ((= x 0) _)
;          ((= y 1) _)
;          (else (A 0
;                   (A 1 9))))
; => (A 0 (A 1 9))
; => (A 0 (A 0 (A 1 8)))
; => ... ; number of 0  + last arg, i.e. `x` in `(A 1 x)` should be 10
; => (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1)))))))))) ; (A 1 1) = 2
; => (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2))))))))) ; (A 0 x) = (* x 2)
; => ... ; how many '0' = how many 2 the result, i.e. `2` in `(A 0 2)` need to multiple by
; => 1024

(out (A 1 10))
; 1024

; => (A 2 4)
; => (A 1 (A 2 3))
; => (A 1 (A 1 (A 2 2)))
; => (A 1 (A 1 (A 1 (A 2 1))))
; => (A 1 (A 1 (A 1 2)))
; => (A 1 (A 1 (A 0 (A 1 1))))
; => (A 1 (A 1 (A 0 2)))
; => (A 1 (A 1 4)) ; (A 1 4) => (A 0 (A 0 (A 0 (A 1 1)))) = 2^4 = 16
; => (A 1 16)
; => 65536 ; 2^16

(out (A 2 4))
; 65536

; => (A 3 3)
; => (A 2 (A 3 2))
; => (A 2 (A 2 (A 3 1)))
; => (A 2 (A 2 2)) ; from above, (A 2 2) = (A 0 2) = 4
; => (A 2 4)
; => 65536

(out (A 3 3))
; 65536

; * mathematical definition of (A 0 n):
; => (A 0 n)
; => (* 2 n)
; definition:
; f n = (A 0 n) = n*2

; * mathematical definition of (A 1 n):
; => (A 1 n)
; => (A 0 (A 1 (- n 1)))
; => ...
; when (A 1 n) reaches (A 1 1), there'll be (n-1) "(A 0 "s on the left and (n-1) ")"s on the right
;     each "(A 0 _)" multiples "_" by 2
; g n = (A 1 n) = 2^n (n > 0)
; when n = 0, g 0 = (A 1 0) = 0
; (A 1 n) will become an infinite loop when n < 0
; definition:
; g n = 2^n (n > 0)
; g 0 = 0

; * mathematical definition: of (A 2 n):
; => (A 2 n)
; => (A 1 (A 2 (- n 1)))
; because h n = (A 2 n), so (A 2 (- n 1)) = h (- n 1)
; we have: h n = (A 1 (h (- n 1))) = g (h (- n 1)) = 2^(h (- n 1)) (n > 0)
; and h 1 = (A 2 1) = 2
; h 1 = 2
; h 2 = 2^(h 1) = 2^2
; h 3 = 2^(h 2) = 2^(2^2)
; ...
; when n = 0
; h 0 = (A 2 0) = 0
; definition:
; h n = 2^(2^(2^(...))) (n > 1) ; we'll have n "2"s on the right side
; h 0 = 0

(define func-compare
  (lambda (f1 f2 testcases)
    (out "test cases:")
    (out testcases)
    (let ((result
            (every (lambda (x) (= (f1 x) (f2 x)))
                   testcases)))
      (if result
        (out "Test passed")
        (out "Test failed")))))
