
(define (valid-signal? s)
  (assert (or (= s 0)
              (= s 1))
          "Invalid signal"))

(define (probe name wire)
  (add-action!
      wire
      (lambda ()
        (format #t
          ; name, current-time, value
          "~%~A ~A  New-value = ~A~%"
          name
          (current-time the-agenda)
          (get-signal wire)))))

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
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
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
