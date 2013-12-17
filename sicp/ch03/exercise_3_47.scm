(load "../common/utils.scm")
(load "../common/test-utils.scm")

; a. in terms of mutexes
(define (semaphore n)
  (let ((m (make-mutex))
        (count n))
    (define (the-semaphore msg)
      (m 'acquire)
      (cond ((eq? msg 'acquire) 
              (if (> count 0)
                ; still can acquire
                (begin
                  (set! count (- count 1))
                  (m 'release))
                ; need to wait
                (begin
                  (m 'release)
                  (the-semaphore 'acquire))))
            ((eq? msg 'release)
              (set! count (+ count 1))
              (m 'release))
            (else
              (begin
                (m 'release)
                (error "Unknown message:" msg)))))
    the-semaphore))

; b. in terms of atomic test-and-test! operations

(end-script)
