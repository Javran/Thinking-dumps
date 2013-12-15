(load "../common/utils.scm")
(load "../common/test-utils.scm")

; primititve synchroization mecahnism:
;   mutex
; supported operations:
; * acquire
;   when acquired, no other acquire operations
;     on that mutex may proceed until the mutex
;     is released
; * release

; need implementation of:
; * make-mutex
; * (<mutex> 'acquire)
; * (<mutex> 'release)

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
              (if (test-and-set! cell)
                (the-mutex 'acquire))) ; retry(warning: busy waiting)
            ((eq? m 'release)
              (clear! cell))))
    the-mutex))

; need implementation of:
; * test-and-set!
; * clear!

; warning: this operation need to be atomic
;   and the implementation here does not guarantee this.
(define (test-and-set! cell)
  (if (car cell)
    true
    (begin
      (set-car! cell true)
      false)))

(end-script)
