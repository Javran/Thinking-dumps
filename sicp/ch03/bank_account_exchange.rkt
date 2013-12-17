#lang racket
; bank account `exchange` implementation:
;   generate some bank accounts and share them
;   among threads, each thread takes some takes of
;   exchanging the balance of two accounts.
;   when the program terminates,
;   we sort the original account balances
;   and the final account balances
;   they should be the same.

; each serializer needs an unique id in our implementation
(define (make-serializer id)
  #f)

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      (error "Insufficient funds")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else
              (error "Unknown request: MAKE-ACCOUNT"
                     m))))
    dispatch))
