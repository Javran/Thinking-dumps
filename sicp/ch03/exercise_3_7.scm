(load "../common/utils.scm")
(load "../common/test-utils.scm")

; obj-holder: store an obj that can be shared
(define (obj-holder obj)
  ; primitives
  (define (run f) (f obj))
  (define (modify! f)
    (set! obj (f obj))
    obj)

  ; readability
  (define (test pred) (run pred))
  (define (get) (run identity))

  (define (dispatch m)
    (cond ((eq? m 'run) run)
          ((eq? m 'modify) modify!)
          ((eq? m 'test) test)
          ((eq? m 'get) get)
          (else (error "Unknown request:"
                       m))))
  dispatch)

; excerpted from ex 3.3, modified
(define (make-account balance password)
  ; use the convention that use exclamation mark
  ;   for methods that have side-effect

  ; initialization:
  (set! balance (obj-holder balance))

  ; methods:
  (define (withdraw! amount)
    (define (suff? balance) (>= balance amount))
    (define (withdraw balance) (- balance amount))
    (if ((balance 'test) suff?)
      (begin
        ((balance 'modify) withdraw)
        ((balance 'get)))
      (begin
        (warn "Insufficient funds")
        #f)
      ))
  (define (deposit! amount)
    ((balance 'modify)
      (lambda (balance) (+ balance amount)))
    ((balance 'get)))
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


(define alice (make-account 1000 'pw))

(out ((alice 'pw 'withdraw) 100))
(out ((alice 'pw 'withdraw) 100))
(out ((alice 'pw 'withdraw) 100))
(out ((alice 'pw 'deposit) 1234))


(end-script)
