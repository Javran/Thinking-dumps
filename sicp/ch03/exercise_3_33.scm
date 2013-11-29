(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./constraint_system.scm")

; a + b = d
; c * 2 = d

(define (averager a b c)
  (let ((d (make-connector))
        (e (make-connector)))
    (constant 2 e)
    (adder a b d)
    (multiplier e c d)
    'ok))

(let ((a (make-connector))
      (b (make-connector))
      (c (make-connector)))
  (averager a b c)
  (set-value! a 100 'user)
  (set-value! b 0   'user)
  (out (get-value c))
  ;  0 + 100 = 50 * 2 => c = 50
  (forget-value! b 'user)
  (forget-value! c 'user)
  (set-value! c 75 'user)
  (out (get-value b))
  ; 100 + 50 = 75 * 2 => b = 50
  )

(end-script)
