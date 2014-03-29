(load "../common/utils.scm")
(load "../common/test-utils.scm")

; agenda abstraction
; * (make-agenda) initialize
; * (empty-agenda? <agenda>)
; * (first-agenda-item <agenda>)
; * (remove-first-agenda-item! <agenda>)
; * (add-to-agenda! <time> <action> <agenda>)
; * (current-time <agenda>)

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

(end-script)
