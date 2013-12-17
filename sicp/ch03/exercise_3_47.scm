(load "../common/utils.scm")
(load "../common/test-utils.scm")

; a. in terms of mutexes
(define (semaphore-a n)
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

; b. in terms of atomic test-and-set! operations
; actually I don't know what makes the difference
;   or is there any difference essentially?

(define (semaphore-b n)
  (let ((cell-count (list #f))
        (count n))
    (define (the-semaphore msg)
      (if (test-and-set! cell-count)
        ; keep waiting
        (the-semaphore msg)
        ; else keep going
        (cond ((eq? msg 'acquire)
                (if (> count 0)
                  ; still can acquire
                  (begin
                    (set! count (- count 1))
                    (clear! cell-count))
                  ; need to wait
                  (begin
                    (clear! cell-count)
                    (the-semaphore 'acquire))))
              ((eq? msg 'release)
                (set! count (+ count 1))
                (clear! cell-count))
              (else
                (begin
                  (clear! cell-count)
                  (error "Unknown message:" msg))))))
    the-semaphore))

(end-script)
