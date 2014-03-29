(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./constraint_system.scm")

(define (squarer a b)
  (multiplier a a b))

(let ((a (make-connector))
      (b (make-connector)))
  (probe 'a a)
  (probe 'b b)
  (squarer a b)
  (set-value! a 3 'user)
  (forget-value! a 'user)
  (forget-value! b 'user)
  (set-value! b 16 'user)
  ; the problem is:
  ; when we change the value of `b`
  ;   the multipliers won't think they have enough
  ;   information to update itself.
  ; here only the modification to `b` is observed
  )

(end-script)
