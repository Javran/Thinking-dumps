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
