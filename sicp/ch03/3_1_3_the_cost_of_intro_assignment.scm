(load "../common/utils.scm")
(load "../common/test-utils.scm")

; programming without any use of assignments
;   is accordingly known as functional programming

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define W (make-simplified-withdraw 25))

(out (W 20))
; 5

(out (W 10))
; -5

; there's no accumulated effect over successive calls
(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

(define D (make-decrementer 25))

(out (D 20))
; 5
(out (D 10))
; 15

(end-script)
