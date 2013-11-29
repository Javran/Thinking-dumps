(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./constraint_system.scm")

(define (celsius-fahrenheit-converter C F)
  ; 9 * C = 5 * (F - 32)
  ; W =   9
  ; X =   5
  ; Y =  32
  ; <LHS> = <RHS> => C*W = U, V*X = U
  ; V = F - 32    => F = V + Y
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define C (make-connector))
(define F (make-connector))

(celsius-fahrenheit-converter C F)

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user)
; here we should have some output

; (set-value! F 212 'user)
; will cause contradiction

(forget-value! C 'user)
(set-value! F 212 'user)
; no problem here

(end-script)
