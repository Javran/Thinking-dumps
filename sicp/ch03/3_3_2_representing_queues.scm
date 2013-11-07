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
  ; make a new pair
  (let ((new-pair (cons item nil)))
    (cond ((empty-queue? queue)
            ; front ptr = rear ptr = new one
            (set-front-ptr! queue new-pair)
            (set-rear-ptr! queue new-pair)
            queue)
          (else
            ; attach new one after rear, update the rear
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))

(define (delete-queue! queue)
  ; move forward once, the first one got "disconnected"
  (set-front-ptr! queue (cdr (front-ptr queue))))

(let ((q (make-queue)))
  ; insert: 1,2,3,4,5
  (for-each ((curry2 insert-queue!) q) '(1 2 3 4 5))
  (let loop ()
    (if (empty-queue? q)
      'done
      (begin
        (out (front-queue q))
        (delete-queue! q)
        (loop))))
  ; delete: 1,2,3,4,5 (FIFO)
  )

(end-script)
