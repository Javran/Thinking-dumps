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

; need implementation of :
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

(end-script)
