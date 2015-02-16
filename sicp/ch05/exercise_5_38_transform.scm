(define (transform-right exp exp-zero)
  (let ((operator (car exp))
        (operands (cdr exp)))
    (reduce-right
     (lambda (i acc)
       (list operator i acc))
     exp-zero
     operands)))

;; a table of transformable function calls
;; whose elements are "(list <op-symbol> <exp-zero>)"s
(define transformable-table
  `((+ 0)
    (* 1)))

(if *ex-5.38-test*
    ;; tests and also examples
    (do-test
     transform-right
     (list
      ;; a regular one
      (mat '(+ 1 2 3 4) 0
           '(+ 1 (+ 2 (+ 3 4))))
      ;; there are two operands already - keep unchanged
      (mat '(* 1 2) 1
           '(* 1 2))
      ;; single operand - remove function call
      (mat '(+ 1) 0
           1)
      ;; called with nothing - this won't usually happen
      ;; but when it happens, we return "exp-zero"
      ;; which should be the "zero" of the corresponding monoid
      (mat '(+) 0
           0)))
    'skipped)

;; Local variables:
;; proc-entry: "./exercise_5_38_tests.scm"
;; End:
