;; all tests in the original metacircular evaluator is abandoned here.
;; for simplicity, we will instead just test the behavior of our evaluator:
;; we reuse tests and the tester in "ec-tests.scm", if the metacircular evaluator
;; agrees with our evaluator in the implementing language, then it is fine.

(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "ec-tests.scm")

(load "metacircular-evaluator.scm")

(set! test-exps
      (append
       test-exps
       `(
         ;; some extra tests
         (or)
         (or #f)
         (or #t (error 'wont-reach))

         (and)
         (and #t)
         (and 1 2 3)
         (and #f (error 'wont-reach))

         (let ((x 1)
               (y 2)
               (z 3))
           (+ x y z))
         (let fib-iter ((a 1) (b 0) (count 10))
           (if (= count 0)
               b
               (fib-iter (+ a b) a (- count 1))))

         (letrec ((f1 (lambda (n)
                        (if (= n 0)
                            1
                            (* n (f2 (- n 1))))))
                  (f2 (lambda (n)
                        (if (= n 0)
                            1
                            (* n (f3 (- n 1))))))
                  (f3 (lambda (n)
                        (if (= n 0)
                            1
                            (* n (f1 (- n 1)))))))
           (f3 10))
         (let* ((x 1)
                (y (+ 1 x))
                (z (* 2 y)))
           (+ x y z))
         (let* ((x 3)
                (y (+ x 2))             ; y = 5
                (z (+ x y 5)))          ; z = 3 + 5 + 5
           (* x z))

         (let ((a 1))
           (cond ((= a 0) => (lambda (x) (if x 10 20)))
                 ((= a 1) => (lambda (x) (if x 30 40)))
                 ((= a 2) 50)
                 (else 60)))

         (let ((a 2))
           (cond ((= a 0) => (lambda (x) (if x 10 20)))
                 ((= a 1) => (lambda (x) (if x 30 40)))
                 ((= a 2) 50)
                 (else 60)))

         (let ((a 5))
           (cond ((= a 0) => (lambda (x) (if x 10 20)))
                 ((= a 1) => (lambda (x) (if x 30 40)))
                 ((= a 2) 50)
                 (else 60)))
         )))

(my-eval-select-approach 'interpret)
(for-each
 (test-evaluator my-eval)
 test-exps) (newline)

(my-eval-select-approach 'analyze)
(for-each
 (test-evaluator my-eval)
 test-exps) (newline)
