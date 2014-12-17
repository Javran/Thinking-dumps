(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu_ec_patch.scm")

;; from now on, we just use "simu.scm" as the machine
;; simulator, as using the legacy one won't make too much difference.

(load "./exercise_5_23_trans.scm")
(load "./exercise_5_23_common.scm")

;; a list of functions that does transformation:
;; * cond->if
;;   converts an cond-exprssion to a nested if-expression
;; * sequence->exp (used internally)
;;   converts a list of expressions into one expression
;;   in which the expressions are evaluated in order and
;;   the result of the last expression returns as the final result
;; * let->combination
;;    converts a let-expression into an application of lambda-expression

(define test-exps
  `(
    ;; test cond
    ;; - normal expressions
    (cond (else 10))
    (cond ((= 1 1) 10)
          ((= 2 1) 20))
    (cond ((= 2 1) 10)
          ((= 1 1) 20))
    (cond (#f 30)
          (else 40))
    ;; - test side effects
    (begin
      (define x 0)
      (cond (#f (set! x 10)
                20)
            (#t (set! x (+ x 5)))
            (else (set! x (+ x 100))))
      x)
    ;; test let
    ;; - normal let-expression
    (let ((x 1)
          (y 2))
      (let ((x 10))
        ;; shadowing
        (+ x y)))
    (let ((x 1))
      (set! x 10)
      (set! x (+ x 200))
      x)
    ;; - name let
    (let loop ((i 0)
               (acc 0))
      (if (> i 10)
          acc
          (loop (+ i 1) (+ acc i))))
    ))

(for-each (test-evaluator machine-eval)
          test-exps)
(newline)

;; Local variables:
;; proc-entry: ""
;; End:
