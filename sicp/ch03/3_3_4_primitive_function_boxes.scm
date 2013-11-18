(load "../common/utils.scm")
(load "../common/test-utils.scm")

; operations on wires:
; * (get-signal <wire>)
; * (set-signal! <wire> <new value>)
; * (add-action! <wire> <procedure of no arguments>)
; * (after-delay <delay time> <procedure of no arguments>)

; from now all primitives are assumed to be exist,
;   we can try build this system now.

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))



(end-script)
