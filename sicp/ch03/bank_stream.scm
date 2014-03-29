(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

; extra task: bank account system
; *  suppose Peter and Paul share a joint bank account.
; *  model their operations as streams
; *  operations are either 'withdraw or 'deposit
; *  for simplicity, assume bank account always has sufficient money
; *  `merge` two streams together to produce a single operation stream

(define (process-bank-op-stream balance ops)
  ; apply the change on balance, and return the modified stream
  (define (apply-change f)
    (define new-balance (f balance))
    (cons-stream
      new-balance
      (process-bank-op-stream
        new-balance
        (tail ops))))
  (if (stream-null? ops)
    the-empty-stream
    (let ((op (head ops)))
      (apply-change
        ; determine the change is either withdraw or deposit
        (cond ((eq? (car op) 'withdraw)
                (lambda (balance) (- balance (cadr op))))
              ((eq? (car op) 'deposit)
                (lambda (balance) (+ balance (cadr op)))))))))

; compact operation representation -> operation representation
(define (cops->ops cops)
  (if (stream-null? cops)
    the-empty-stream
    (let ((op (head cops)))
      (cons-stream
        (list op (head (tail cops)))
        (cops->ops (tail (tail cops)))))))
(define (ops->cops ops)
  (if (stream-null? ops)
    the-empty-stream
    (let ((op (head ops)))
      (cons-stream
        (car op)
        (cons-stream
          (cadr op)
          (ops->cops (tail ops)))))))

; same as `process-bank-op-stream` but this one deals with
;   compact operation streams
(define (process-bank-compact-op-stream balance cops)
  (process-bank-op-stream
    balance
    (cops->ops cops)))

(define (merge-cops cops1 cops2)
  (ops->cops
    (interleave
      (cops->ops cops1)
      (cops->ops cops2))))

(define peter-cops
  ; net = - 10 + 20 - 30 + 40 = 20
  (list->stream
    '(withdraw 10
      deposit 20
      withdraw 30
      deposit 40)))

(define paul-cops
  ; net = + 100 - 50 - 30 + 80 = 100
  (list->stream
    '(deposit 100
      withdraw 50
      withdraw 30
      deposit 80)))

(display-stream
  (process-bank-compact-op-stream
    ; initialize balance with 100
    100
    ; net = 20 + 100 = 120
    (merge-cops
      peter-cops
      paul-cops)))
; the final balance should be 100 + 120 = 220

(end-script)
