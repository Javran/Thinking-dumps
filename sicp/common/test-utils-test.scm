(load "./utils.scm")
(load "./test-utils.scm")

(let ((testcases (list (cons (list 1 2 3) 6)
                       (cons (list 4 5 6) 17)))
      (on-correct (lambda (testcase)
                    (out "correct: " testcase)))
      (on-wrong (lambda (testcase actual)
                  (out "wrong: " testcase)
                  (out "actual: " actual))))
  (do-test-ex + testcases = on-correct on-wrong))

(end-script)
