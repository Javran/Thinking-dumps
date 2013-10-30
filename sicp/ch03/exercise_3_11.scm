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

(out ((acc 'withdraw) 60))
#|
; the lambda is called, new environment
; E4 <- {m= 'withdraw} : [E1, G]
(cond ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else
        (error "Unknown request: MAKE-ACCOUNT" m)))
; fall into the first case, evaluate `withdraw`, which is
(define (withdraw amount)
  (if (>= balance amount)
    (begin (set! balance (- balance amount))
           balance)
    "Insufficient funds"))
; the lambda is called, new environment.
; E5 <- {amount = 60} : [E4, E1, G]
(begin (set! balance (- balance amount))
       balance)
; the closest balance binding found in E1,
; [E5, E4, E1 <- {balance = 30}, G]
; re-evaluate `balance`, get 30
|#

; Where is the local state for acc kept?
;
;   it's E1, the environment created when `make-account` is called

; Suppose we define another account, `(define acc2 (make-account 100))`
;   How are the local states for the two accounts kept distinct?
;
;   Whenever `make-account` is called, it creates a new environment.
;   This environment is accessible by the functions defined in this environment,
;   i.e. `dispatch`, `withdraw`, `deposit`. Exposing `dispatch` makes it possible
;   for any calls outside this environment to modify this environment indirectly.
;   What we need to do is just storing the returned instance (i.e. `dispatch`) 
;   somewhere for future use.

; Which parts of the environment structure are shared between acc and acc2?
;
;   Only global environment.

(end-script)
