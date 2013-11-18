(load "../common/utils.scm")
(load "../common/test-utils.scm")

; operations on wires:
; * (get-signal <wire>)
; * (set-signal! <wire> <new value>)
; * (add-action! <wire> <procedure of no arguments>)
; * (after-delay <delay time> <procedure of no arguments>)

; from now all primitives are assumed to be exist,
;   we can try build this system now.

(define (valid-signal s)
  (cond ((= s 0) #t)
        ((= s 1) #t)
        (else #f)))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (logical-not s)
  (assert (valid-signal s))
  (cond ((= s 0) 1)
        ((= s 1) 0))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  ; well.. what if a1 and a2 are changing simutaneously?
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and a b)
  (assert (valid-signal a))
  (assert (valid-signal b))
  (if (and (= a 1) (= b 1))
    1
    0))

(end-script)
