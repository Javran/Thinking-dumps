(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./3_3_2_representing_queues_procs.scm")

(define q1 (make-queue))
(insert-queue! q1 'a)
; make a new pair: (cons 'a nil)
; => (a)
; front-ptr = a
; rear-ptr = a
; => (cons (cons 'a nil) (cons 'a nil))
; because: '(a b) = (list 'a 'b) = (cons 'a (cons 'b nil))
; => ((a) a)
(out q1)

(insert-queue! q1 'b)
; make a new pair: (cons 'b nil)
; (cdr a) set to (cons 'b nil)
; => ((a b) a b)
; then move rear-ptr
; => ((a b) b)

(out q1)

(delete-queue! q1)
; front-ptr move forward
; ((b) b)
(out q1)

(delete-queue! q1)
; front-ptr move forward
; (() b)
(out q1)

; because the queue is indicated by the list
;   that begins from the point where `front-ptr`
;   points to what `rear-ptr` points to doesn't matter.
; moveover, when the queue is empty, `rear-ptr` will be reset
;   so the correctness is guaranteed

(newline)
(define print-queue (compose out car))
(define q2 (make-queue))
(insert-queue! q2 'a)
(print-queue q2)

(insert-queue! q2 'b)
(print-queue q2)

(delete-queue! q2)
(print-queue q2)

(delete-queue! q2)
(print-queue q2)

(end-script)
