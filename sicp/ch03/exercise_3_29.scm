(load "../common/utils.scm")
(load "../common/test-utils.scm")

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
  (assert (valid-signal a1))
  (assert (valid-signal a2))
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

; TODO: need ways to verify

(end-script)
