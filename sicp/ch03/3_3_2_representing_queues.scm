(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./3_3_2_representing_queues_procs.scm")
; queue abstraction:
; constructor:
;   (make-queue)
; selector:
;   (empty-queue? q)
;   (front-queue q)
; mutator:
;   (insert-queue! q i)
;   (delete-queue! q)

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
