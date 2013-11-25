(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./circuit_simulate.scm")

(define (make-wire)
  (let (; the initial signal is 0
        (signal-value 0)
        ; we need to keep a list
        ;   when state changes, they should happen in chain
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
        ; trigger procedures
        (begin (set! signal-value new-value)
               (call-each action-procedures))
        ; nothing happens if the signal is unchanged
        'done))
    ; fine, that's exactly the problem I want to ask
    (define (accept-action-procedure! proc)
      (set! action-procedures
        (cons proc action-procedures))
      ; (proc) ---> what if we don't have this line here?
      'done)
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (circuit-test)
  (define input-1 (make-wire))
  (define input-2 (make-wire))
  (define sum (make-wire))
  (define carry (make-wire))

  (probe 'sum sum)
  (probe 'carry carry)

  (half-adder input-1 input-2 sum carry)

  (set-signal! input-1 1)

  (out "First propagate")
  (propagate) 

  (set-signal! input-2 1)
  (out "Second propagate")
  (propagate)
  )

(circuit-test)
; trace back:
; (proc) -> what is `proc`?
; (accept-action-procedure! proc) ->
; (add-action! wire action-procedures) ->
; (after-delay delay proc) ->
; (add-to-agenda! time action agenda)

; so I guess the reason is to put at least one action
;   on the agenda so the whole circuit can proceed to run

; I still think this is a dirty hack however:
; if we don't have the action run first time it was put on agenda
;   we'll get a "lazy" circuit -- as long as new signal does not
;   affect the value of other wires, the circuit does nothing,
;   which is exactly the behavior of "lazyness".
; here I have two tests to demostrate what I'm saying:
;   for these two tests, you will see that the circuit acts normally.
; the problem in the example above, I guess is because of the inverter:
;   we haven't consider the problem that when an inverter is put in,
;   the output value should be immediately the logical not of the input
;   signal.

(out "test #1")
(let ()
  (define a (make-wire))
  (define b (make-wire))
  (define c (make-wire))
  (and-gate a b c)
  (probe 'res c)

  ; for and-gate
  ;   the probe triggered when a = 1, b = 1
  (out "a -> 1")
  (set-signal! a 1)
  (propagate)
  (out "b -> 1")
  (set-signal! b 1)
  (propagate)

  ; and as soon as a = 0, 
  ;   the probe triggered again.
  (out "a -> 0")
  (set-signal! a 0)
  (propagate)
  (out "b -> 0")
  (set-signal! b 0)
  (propagate)
  )

(out "test #2")
(let ()
  (define a (make-wire))
  (define b (make-wire))
  (define c (make-wire))
  (or-gate a b c)
  (probe 'res c)

  ; for or-gate
  ;   the probe triggered when a = 1
  (out "a -> 1")
  (set-signal! a 1)
  (propagate)
  (out "b -> 1")
  (set-signal! b 1)
  (propagate)

  (out "a -> 0")
  (set-signal! a 0)
  (propagate)
  ; when a = 0, b = 0
  ;   the probe triggered again.
  (out "b -> 0")
  (set-signal! b 0)
  (propagate)
  )

; if we make the inverter send the signal change immediately
;   after it is set up, the problem should be solved.
; and we actually really don't need to call the action,
;   immediately.
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  (after-delay
    0
    (lambda ()
      (set-signal!
        output
        (logical-not (get-signal input)))))
  'ok)

(out "re-test")

(set! the-agenda (make-agenda))
(circuit-test)

(end-script)
