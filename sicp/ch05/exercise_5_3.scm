(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_5_5_common.scm")

(load "./exercise_5_3_controller.scm")

;; fix x = 144
(define test-const 144)

(define x test-const)
(define (good-enough? guess)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess)
  (average guess (/ x guess)))

;; test sqrt-machine-v1
(out "sqrt-machine-v1:")
(ms-pretty-print
 (execute-controller-with-regs
  sqrt-machine-v1
  `((x ,test-const))))
(newline)(newline)

(set! good-enough? #f)

;; test sqrt-machine-v2
(out "sqrt-machine-v2:")
(ms-pretty-print
 (execute-controller-with-regs
  sqrt-machine-v2
  `((x ,test-const))))
(newline)(newline)

(set! improve #f)
(set! x #f)

;; test sqrt-machine-v3
(out "sqrt-machine-v3:")
(ms-pretty-print
 (execute-controller-with-regs
  sqrt-machine-v3
  `((x ,test-const))))
(newline)(newline)
