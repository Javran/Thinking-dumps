(load "../common/utils.scm")
(load "../common/test-utils.scm")

; excerpted from ex 3.3
(define (make-account balance password)
  ; use the convention that use exclamation mark
  ;   for methods that have side-effect
  (define (withdraw! amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      (begin
        (warn "Insufficient funds")
        #f)
      ))
  (define (deposit! amount)
    (set! balance (+ balance amount))
    balance)
  (define (change-password! new-password)
    (set! password new-password))
  (define (dispatch try-password m)
    (if (eq? password try-password)
      (cond ((eq? m 'withdraw) withdraw!)
            ((eq? m 'deposit) deposit!)
            ((eq? m 'change-password) change-password!)
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m)))
      (lambda args (warn "Incorrect password"))))
  dispatch)

; some modification required.
; * should make the amount store in an environment that can be shared
;   because I made an extension that enables us to change the password
;   we have to do something like this in case that changing the password
;   of one account has side-effect on another account.


(end-script)
