(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_3_18_utils.scm")

; here I use Floyd's cycle-finding algorithm:
; http://en.wikipedia.org/wiki/Cycle_detection#Tortoise_and_hare

(define (contains-cycle? x)
  (define (next x)
    (if (null? x)
      nil
      (cdr x)))
  (let loop ((slower (next x))
             (faster (next (next x))))
    (if (eq? slower faster)
      (not (null? slower))
      (loop (next slower) (next (next faster))))))

(contains-cycle-test contains-cycle?)

(end-script)
