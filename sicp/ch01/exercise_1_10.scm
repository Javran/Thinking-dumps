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
