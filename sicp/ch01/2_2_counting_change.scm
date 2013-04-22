(load "../common/utils.scm")

; half-dollars  0.5  (50)
; quarters      0.25 (25)
; dimes         0.1  (10)
; nickels       0.05 ( 5)
; pennies       0.01 ( 1)

(define (coin-kind->denomination kind)
  (cond ((= kind 1)  1)
        ((= kind 2)  5)
        ((= kind 3) 10)
        ((= kind 4) 25)
        ((= kind 5) 50)
        (else (error "unknown coin kind"))))

(define (count-change amount)
  (define (count-change-with-kinds amount kind-list)
    (if (null? kind-list)
      ; no more alternative in kind-list
      0
      ; else
      (let ((current-kind (car kind-list))
            (rest-kind (cdr kind-list)))
        (cond ((= amount 0) 1) ; if a is exactly 0, we have 1 way.
              ((< amount 0) 0) ; if a is less than 0, we have no way.
              (else (+ (count-change-with-kinds 
                         amount
                         rest-kind)
                       (count-change-with-kinds
                         (- amount (coin-kind->denomination current-kind))
                         kind-list)))))))
  (count-change-with-kinds amount '(1 2 3 4 5)))

(out (count-change 100))
; 292
