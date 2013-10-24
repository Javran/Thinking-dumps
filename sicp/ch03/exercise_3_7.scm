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
  ;   for numbers, wrap them inside
  ;   for anything else, just set it to balance
  (if (number? balance)
    (set! balance (obj-holder balance))
    (set! balance balance))

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
        #f)))
  (define (deposit! amount)
    ((balance 'modify)
      (lambda (balance) (+ balance amount)))
    ((balance 'get)))
  (define (change-password! new-password)
    (set! password new-password))

  (define (joint joint-account-password)
    (make-account balance joint-account-password))

  (define (show)
    ((balance 'get)))

  (define (dispatch try-password m)
    (if (eq? password try-password)
      (cond ((eq? m 'withdraw) withdraw!)
            ((eq? m 'deposit) deposit!)
            ((eq? m 'show) show)
            ((eq? m 'change-password) change-password!)
            ((eq? m 'joint) joint)
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m)))
      (lambda args (warn "Incorrect password"))))
  dispatch)

(define (make-joint origin-acc origin-pw joint-pw)
  ((origin-acc origin-pw 'joint) joint-pw))

(define peter-acc (make-account 1000 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

(out ((peter-acc 'open-sesame 'show)))
; 1000
(out ((peter-acc 'rosebud 'show)))
; Incorrect password
(out ((paul-acc 'open-sesame 'show)))
; Incorrect password
(out ((paul-acc 'rosebud 'show)))
; 1000

(newline)
(out ((peter-acc 'open-sesame 'withdraw) 500))
; 500
(out ((paul-acc 'rosebud 'show)))
; 500
(out ((paul-acc 'rosebud 'deposit) 734))
; 1234
(out ((peter-acc 'open-sesame 'show)))
; 1234

(end-script)
