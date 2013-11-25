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

; an instance of `agenda` is kept to implement the event-driven part
;   the agenda tells us that is the next step.
;   while we are doing operations on agenda, procedures keeps being added
;   according to what is happening, and thus keep the whole process going

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

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

; DON'T USE THESE CONFUSING STUFFS `(list 0)` WHEN
;   WHAT YOUR ARE DOING IS TO CREATE A PAIR
;   RATHER THAN A LIST

; make an agenda with time=0, no time segments
(define (make-agenda) (cons 0 nil))
(define current-time car)
(define segments cdr)

(define set-current-time! set-car!)
(define set-segments! set-cdr!)

(define first-segment (compose car segments))
(define rest-segments (compose cdr segments))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define empty-agenda? (compose null? segments)

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty: FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda
                         (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

(define (add-to-agenda! time action agenda)
  ; insert-inplace? tests if the given action should be
  ;   insert in front of the `segments`
  (define (insert-inplace? segments)
        ; if `segments` is empty
    (or (null? segments)
        ; or if the first segment from `segments`
        ;   has a newer (greater) time than
        ;   the action we want to add into
        (< time (segment-time (car segments)))))
  ; make a time segment using `time` and `action`
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      ; the head is exactly the time we want
      ;   put the action into that queue
      (insert-queue! (segment-queue (car segments))
                     action)
      ; else
      (let ((rest (cdr segments)))
        (if (insert-inplace? rest)
          ; if we should insert in place,
          ;   make the segment and put it here
          (set-cdr!
            segments
            (cons (make-new-time-segment time action)
                  (cdr segments)))
          ; else this is not the right place,
          ;   keep going
          (add-to-segments! rest)))))
  ; so what's the benefit of confusing others with
  ;   shadowing the definition of `segments` here?
  (let ((segments (segments agenda)))
    (if (insert-inplace? segments)
      (set-segments!
        agenda
        (cons (make-new-time-segment time action)
              segments))
      (add-to-segments! segments))))

(end-script)
