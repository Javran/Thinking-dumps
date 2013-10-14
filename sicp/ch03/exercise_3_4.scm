(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (call-the-cops)
  (out "`call-the-cops` invoked."))

; make a trigger, when the internal state satisfies `fire?`,
;   `callback` should be called
; init-val : initial internal state
; fire?    : return #t if we need to trigger the callback
; next     : an unary that produces the next state
; callback : when `fire?` returned true, `callback` will be invoked.
(define (trigger init-val fire? next callback)
  (let ((val init-val))
    (define (set x)
      (set! val x)
      (if (fire? val)
        (callback)
        'done))
    (define (reset-val)
      (set init-val))
    (define (next-val)
      (set (next val)))
    (define (dispatch m)
      (cond ((eq? m 'reset) (reset-val))
            ((eq? m 'next) (next-val))))
    dispatch))

(define x (trigger
            0
            ((curry2 <=) 3)
            inc
            call-the-cops))

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             (out balance))
      (out "Insufficient funds")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    (out balance))
  ; support password changing in addition
  ;   given that `change-password` can only be called when
  ;   the old password has been confirmed (by `dispatch`)
  ;   so here we don't need the old password
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

(end-script)
