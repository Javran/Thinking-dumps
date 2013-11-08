(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./3_3_2_representing_queues_procs.scm")

(define q1 (make-queue))
(insert-queue! q1 'a)
(out q1)

(insert-queue! q1 'b)
(out q1)

(delete-queue! q1)
(out q1)

(delete-queue! q1)
(out q1)

(end-script)
