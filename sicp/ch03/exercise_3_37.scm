(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./constraint_system.scm")

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    ; y + z = x
    (adder y z x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    ; y * z = x
    (multiplier y z x)
    z))

(define (cv c)
  (let ((z (make-connector)))
    (constant c z)
    z))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(let* ((C (make-connector))
       (F (celsius-fahrenheit-converter C)))
  (set-value! C 0 'user)
  (out (get-value F))
  ; 32
  (forget-value! C 'user)
  (forget-value! F 'user)
  (set-value! F 212 'user)
  (out (get-value C))
  ; 100
  )

(let* ((a (make-connector))
       (b (c- (cv 10) a)))
  ; 10 - a = b
  (set-value! a 2 'user)
  (out (get-value b))
  ; 8
  (forget-value! a 'user)
  (forget-value! b 'user)
  (set-value! b 4 'user)
  (out (get-value a))
  ; 6
  )

(end-script)
