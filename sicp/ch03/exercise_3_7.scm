(load "../common/utils.scm")
(load "../common/test-utils.scm")

; excerpted from ex 3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             (out balance))
      (out "Insufficient funds")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    (out balance))
  (define (change-password new-password)
    (set! password new-password))
  (define (dispatch try-password m)
    (if (eq? password try-password)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'change-password) change-password)
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m)))
      (lambda args (out "Incorrect password"))))
  dispatch)

(define acc (make-account 100 'secret-password))

(end-script)
