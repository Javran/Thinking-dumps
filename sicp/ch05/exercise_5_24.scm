(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "simu_ec_patch.scm")

(load "exercise_5_23_common.scm")
(load "exercise_5_24_common.scm")
(load "exercise_5_24_machine.scm")

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
    (cond (#f 10)
          (#t 20))
    (cond ((= (+ 10 20) 10) 1)
          ((= (+ 10 20) 20) 2)
          ((= (+ 10 20) 30) 3)
          (else 4))
    (begin
      (define x 10)
      ;; in general I think it's not recommended
      ;; to have side effects when evaluating the condition
      ;; here we are just testing the functionality
      (cond ((begin
               (set! x (+ x 1))
               (= x 12)) 10)
            ((begin
               (set! x (+ x 1))
               (= x 12)) 20)
            (else 30)))
    (begin
      (define y 10)
      (cond ((= 1 2) (set! y (+ y 10)))
            ((= 1 1) (set! y (+ y 100))))
      y)
    (begin
      (define (f x y z)
        (+ x y z))
      (cond ((= (f 1 2 3) 5) 1)
            ((= (f 1 2 3) 6) 2)))
    ))

(for-each (test-evaluator machine-eval) test-exps)
(newline)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
