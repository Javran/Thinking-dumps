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

; subsitution model analysis on `(D 20)`
; (D 20)
; => ((make-decrementer 25) 20)
; => ((lambda (amount) (- 25 amount)) 20)
; => (- 25 20)
; => 5

; subsitution model analysis on `(W 20)`
; (W 20)
; => ((make-simplified-withdraw 25) 20)
; => ((lambda (amount)
;      (set! balance (- 25 amount))
;      25) 20)
; => (set! balance (- 25 20) 25
; => (set! balance 5) 25
; *WRONG ANSWER*

; we would have to distinguish the first occurence of `balance`
;   from the second occurence of `balance`, and the subsitution
;   model cannot do this.

; subsitution model is base ultimately on the notion that
;   the symbols in our language are essentially names for values.

(end-script)
