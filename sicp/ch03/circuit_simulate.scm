(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))
(define (valid-signal? s)
  (assert (or (= s 0)
              (= s 1))
          "Invalid signal"))

(define (logical-not s)
  (valid-signal? s)
  (cond ((= s 0) 1)
        ((= s 1) 0)))

(define (logical-and a1 a2)
  (valid-signal? a1)
  (valid-signal? a2)
  (if (and (= a1 1)
           (= a2 1))
    1 0))

(define (logical-or a1 a2)
  (valid-signal? a1)
  (valid-signal? a2)
  (if (and (= a1 0)
           (= a2 0))
    0 1))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (add-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1)
                         (get-signal a2))))
      ; wait some amount of time (simulating the delay)
      ;   and then trigger the signal change on output
      ; I think here we might trigger the output change twice
      ;   because a1 and a2 will try to put the signal on output
      ;   independently. But as long as our wire only trigger events
      ;   when it notices a change in signal, I think it should be fine.
      (after-delay
        and-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a1)
                        (get-signal a2))))
      (after-delay
        or-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

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
    (define (accept-action-procedure! proc)
      (set! action-procedures
        (cons proc action-procedures))
      ; I can't see the point here
      ;   to run the procedure just inserted
      ;   regardless of the wire-state
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin ((car procedures))
           (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedures)
  ((wire 'add-action!) action-procedures))

; missing definition: after-delay, inverter-delay
; missing definition: and-gate-delay, or-gate-delay

(end-script)
