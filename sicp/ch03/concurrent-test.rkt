#lang racket/load

(load "./concurrent.rkt")

(define res (make-semaphore 1))
(define x 0)

(define prod1run 0)
(define prod2run 0)
(define cons1run 0)
(define cons2run 0)

(define keep-going #t)

(define tds
  (parallel-execute
    (lambda ()
      (let loop ()
        (semaphore-wait res)
        (set! x (+ x 1))
        (set! prod1run (+ prod1run 1))
        (semaphore-post res)
        (if keep-going (loop) 'done)))
    (lambda ()
      (let loop ()
        (semaphore-wait res)
        (set! x (+ x 1))
        (set! prod2run (+ prod2run 1))
        (semaphore-post res)
        (if keep-going (loop) 'done)))
    (lambda ()
      (let loop ()
        (semaphore-wait res)
        (set! x (- x 1))
        (set! cons1run (+ cons1run 1))
        (semaphore-post res)
        (if keep-going (loop) 'done)))
    (lambda ()
      (let loop ()
        (semaphore-wait res)
        (set! x (- x 1))
        (set! cons2run (+ cons2run 1))
        (semaphore-post res)
        (if keep-going (loop) 'done)))))

(sleep 1)
(set! keep-going #f)
(for-each
  thread-wait
  tds)

(display
  (format "equality: ~A <-> ~A~%data: ~A~%"
          (+ prod1run prod2run)
          (+ x cons1run cons2run)
          (list prod1run prod2run cons1run cons2run x)))
