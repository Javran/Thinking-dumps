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
      (proc)
      'done)
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (call-each procedures)
  (for-each (lambda (x) (x)) procedures)
  ; or, point-free style:
  ; (for-each ((curry2 (flip apply)) nil) procedures)
  )

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedures)
  ((wire 'add-action!) action-procedures))
