(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./circuit_simulate.scm")

(define (valid-signal s)
  (cond ((= s 0) #t)
        ((= s 1) #t)
        (else #f)))

(define (logical-or a b)
  (assert (valid-signal a))
  (assert (valid-signal b))
  (if (and (= a 0) (= b 0))
    0
    1))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

; you might need to complete
;   the all chapters about circuit simulation
;   before you can use that.

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))

(probe 'out-probe c)

(or-gate a b c)

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
