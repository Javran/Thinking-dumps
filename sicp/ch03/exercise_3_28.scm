(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (valid-signal s)
  (cond ((= s 0) #t)
        ((= s 1) #t)
        (else #f)))

(define (logical-or a b)
  (assert (valid-signal a))
  (assert (valid-signal b))
  (if (and (= a 0) (= b 0))
    0
    1))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

; TODO: no way to test for a while...

(end-script)
