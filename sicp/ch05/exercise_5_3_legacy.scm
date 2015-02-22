;; this is the "simu" version of the simulator
;; in action
(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "exercise_5_3_controller.scm")

(load "legacy-easy.scm")

(define (good-enough? guess)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess)
  (average guess (/ x guess)))

(define test-const 144)
(define x test-const)

(out "sqrt-machine-v1:")
(let ((m (make-and-execute-with
          sqrt-machine-v1
          `((x ,test-const))
          (concat
           (list (default-primitive-list)
                 `((good-enough? ,good-enough?)
                   (improve ,improve)))))))
  (out (get-register-contents m 'result)))
(newline)

;; without "good-enough?"
(out "sqrt-machine-v2:")
(let ((m (make-and-execute-with
          sqrt-machine-v2
          `((x ,test-const))
          (concat
           (list (default-primitive-list)
                 `((improve ,improve)))))))
  (out (get-register-contents m 'result)))
(newline)

(out "sqrt-machine-v3:")
(let ((m (make-and-execute
          sqrt-machine-v3
          `((x ,test-const)))))
  (out (get-register-contents m 'result)))
