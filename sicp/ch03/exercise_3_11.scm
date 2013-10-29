(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else
            (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)

(define acc (make-account 50))

#|
; make-account -> create a new environment on the top of global one G
; E1 <- {balance=50} : G
; the code is now:
(define (withdraw amount)
  (if (>= balance amount)
    (begin (set! balance (- balance amount))
           balance)
    "Insufficient funds"))
(define (deposit amount)
  (set! balance (+ balance amount))
  balance)
(define (dispatch m)
  (cond ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        (else
          (error "Unknown request: MAKE-ACCOUNT" m))))
dispatch
; E1 <- {withdraw, deposit, dispatch} : G
(lambda (amount)
  (cond ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        (else
          (error "Unknown request: MAKE-ACCOUNT" m))))
|#
; look at the code here
(pp acc)

(out ((acc 'deposit) 40))
; 90
#|
; the lambda is called, new environment.
; E2 <- {m = 'deposit} : [E1, G]
(cond ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else
        (error "Unknown request: MAKE-ACCOUNT" m)))
; fall into the second case, evaluate `deposit`, which is
(lambda (amount)
  (set! balance (+ balance amount))
  balance)
; whose environment is [E2, E1, G]
; now consider ((acc 'deposit) 40)
; the lambda is called, new environment.
; E3 <- {amount = 40} : [E2, E1, G]
(set! balance (+ balance amount))
balance
; (+ balance amount) evaluates to (+ 50 40) = 90
; the closest balance binding found in E1
; [E3, E2, E1 <- {balance = 90}, G]
; re-evaluate `balance`, get 90.
|#

(end-script)
