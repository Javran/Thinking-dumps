(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./circuit_simulate.scm")

; truth table:
; a b  '!a and !b' 'a or b'
; 1 1  0            1
; 1 0  0            1
; 0 1  0            1
; 0 0  1            0

; (logical-or a b) = (logical-not
;                      (logical-and
;                        (logical-not a)
;                        (logical-not b)))


#|
                   ------
a -> o -> wire1 -> |      \
                   |  and  |-> wire3 -> o ->
b -> o -> wire2 -> |      /
                   ------
|#

(define (valid-signal s)
  (cond ((= s 0) #t)
        ((= s 1) #t)
        (else #f)))

(define (or-gate a1 a2 output)
  (let ((wire1 (make-wire))
        (wire2 (make-wire))
        (wire3 (make-wire)))
    (inverter a wire1)
    (inverter b wire2)
    (and-gate wire1 wire2 wire3)
    (inverter wire3 output)
    'ok))

; if signal is processed parallelly,
;   or-gate-delay = inverter-delay*2 + and-gate-delay
; else
;   or-gate-delay = inverter-delay*3 + and-gate-delay

; you might need to complete
;   the all chapters about circuit simulation
;   before you can use that.

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))

(probe 'out-probe c)

(or-gate a b c)

; this is more verbose than ex 3.28, I guess that's because
;   this one combines other circuits and has some "unstable" states

(out "a -> 1")
(set-signal! a 1)(propagate)
; change to 1

(out "b -> 1")
(set-signal! b 1)(propagate)
; nop

(out "a -> 0")
(set-signal! a 0)(propagate)
; nop

(out "b -> 0")
(set-signal! b 0)(propagate)
; change to 0

(end-script)
