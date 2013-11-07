(load "../common/utils.scm")
(load "../common/test-utils.scm")

; queue abstraction:
; constructor:
;   (make-queue)
; selector:
;   (empty-queue? q)
;   (front-queue q)
; mutator:
;   (insert-queue! q i)
;   (delete-queue! q)

; seems like primitives
(define front-ptr car)
(define rear-ptr cdr)
(define set-front-ptr! set-car!)
(define set-rear-ptr! set-cdr!)

(define empty-queue? (compose null? front-ptr))
(define (make-queue) (cons '() '()))

; just let it crash
(define front-queue (compose car front-ptr))

(define (insert-queue! queue item)
  (let ((new-pair (cons item nil)))
    (cond ((empty-queue? queue)
            (set-front-ptr! queue new-pair)
            (set-rear-ptr! queue new-pair)
            queue)
          (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))

(define (delete-queue! queue)
  (set-front-ptr! queue (cdr (front-ptr queue))))

(end-script)
