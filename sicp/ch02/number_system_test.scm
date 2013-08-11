(load "../common/utils.scm")
(load "../common/test-utils.scm")
(load "./tag_system.scm")
(load "./number_system.scm")

(run-all-test)

; ==== demo ===
(define (out-num x)
  (out (to-string (drop x))))
; (sqrt 3)+1i + 5 - (2 cos(pi/6) + 2 sin(pi/6) i) = 5
(out-num (sub (add (make-complex-ri (sqrt 3) 1) (make-scheme-number 5))
              (make-complex-ma 2 (/ pi 6))))
; (2+2i) * (2-2i) = 8
(out-num (mul (make-complex-ri 2 2)
              (make-complex-ri 2 -2)))

; 8 / (2+2i) = 2-2i
(out-num (div (make-rational 64 8)
              (make-complex-ma (sqrt 8) (/ pi 4))))

; (8+4i) / (2+i) = 4
(out-num (div (make-complex-ri 8 4)
              (make-complex-ri 2 1)))

(end-script)
