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
    'todo
    (define (dispatch m)
      'todo)
    dispatch))

(define (empty-queue? q)
  ((q 'empty?)))

(define (front-queue q)
  ((q 'front)))

(define (insert-queue! q i)
  ((q 'insert!) i))

(define (delete-queue! q)
  ((q 'delete!)))


(end-script)
