(load "../common/utils.scm")
(load "../common/test-utils.scm")

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

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

; does anyone think this makes sense to test
;   these stuffs with so many variables undefined??!

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)
(propagate)

(set-signal! input-2 1)
(propagate)

(end-script)
