(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./constraint_system.scm")

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
      (if (< (get-value b) 0)
        (error "square less than 0: SQUARER"
               (get-value b))
        (set-value! a (sqrt (get-value b)) me))
      ; else
      (if (has-value? a)
        (set-value! b (square (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
            (process-new-value))
          ((eq? request 'I-lost-my-value)
            (process-forget-value))
          (else (error "Unknown request: SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

(let ((a (make-connector))
      (b (make-connector)))
  (probe 'a a)
  (probe 'b b)
  (squarer a b)
  (set-value! a 3 'user)
  (forget-value! a 'user)
  (forget-value! b 'user)
  (set-value! b 16 'user)
  )

(end-script)
