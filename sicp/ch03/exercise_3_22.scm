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

(define (make-queue)
  (let ((front-ptr nil)
        (rear-ptr nil))
    (define (empty?)
      (null? front-ptr))
    (define (front)
      (car front-ptr))
    (define (insert! i)
      (let ((p (cons i nil)))
        (if (empty?)
          (begin
            (set! front-ptr p)
            (set! rear-ptr p))
          (begin
            (set-cdr! rear-ptr p)
            (set! rear-ptr p)))))
    (define (delete!)
      (set! front-ptr (cdr front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'empty?) empty?)
            ((eq? m 'front) front)
            ((eq? m 'insert!) insert!)
            ((eq? m 'delete!) delete!)))
    dispatch))

(define (empty-queue? q)
  ((q 'empty?)))

(define (front-queue q)
  ((q 'front)))

(define (insert-queue! q i)
  ((q 'insert!) i))

(define (delete-queue! q)
  ((q 'delete!)))

(let ((q (make-queue)))
  (for-each
    ((curry2 insert-queue!) q)
    '(1 2 3 4 5))
  (define (pop-print-all q)
    (let loop ()
      (if (empty-queue? q)
        'done
        (begin 
          (out (front-queue q))
          (delete-queue! q)
          (loop)))))
  (pop-print-all q)
  ; 1,2,3,4,5
  (for-each
    ((curry2 insert-queue!) q)
    '(7 1 2 3))

  (delete-queue! q)
  (insert-queue! q 4)
  (newline)
  (pop-print-all q)
  ; 1,2,3,4
  )

(end-script)
