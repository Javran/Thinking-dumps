(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./3_3_2_representing_queues_procs.scm")

(load "./circuit_simulate_wire.scm")
(load "./circuit_simulate_gate.scm")
(load "./circuit_simulate_agenda.scm")

; an instance of `agenda` is kept to implement the event-driven part
;   the agenda tells us that is the next step.
;   while we are doing operations on agenda, procedures keeps being added
;   according to what is happening, and thus keep the whole process going
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

; `the-agenda` mainpulation
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
    ; until there's nothing left
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      ; get the first item (procedure) and run it.
      (first-item)
      ; remove and keep going
      (remove-first-agenda-item! the-agenda)
      (propagate))))
