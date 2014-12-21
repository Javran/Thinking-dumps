(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu_ec_patch.scm")


(load "./exercise_5_24_common.scm")
(load "./exercise_5_24_machine.scm")

;; the problem becomes really complicated
;; when it comes to dealing with invalid cases
;; and for now on let's just assume the expression given
;; is always valid and not to worry too much about error
;; handling. this would save my life a lot.

;; let's just test those valid expressions
(define test-exps
  `(
    (cond (else (define x 10)
                (+ x x 1)))
    (cond (else 20))
    (cond (else (define x 10)
                (define y (+ x 10))
                (+ x y y)))
    (cond (#t 10)
          (else 20))
    ))

(for-each (test-evaluator machine-eval) test-exps)
(newline)
;; TODO some tests

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
