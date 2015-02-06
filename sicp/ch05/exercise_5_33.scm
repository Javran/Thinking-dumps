(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./compiler.scm")

;; generating factorial-alt
(for-each
 out
 (statements
  (compile-and-check
   ;; renamed to factorial
   ;; for better diff output
   `(define (factorial n)
      (if (= n 1)
          1
          (* n (factorial (- n 1))))))))

;; see "./execrise_5_33_factorial-alt.scm"
;; for output

;; see "./exercise_5_33.diff" for comparison

;; the only difference in the expressions is that
;; "factorial-alt" (a.k.a the "factorial" in this file")
;; in the alternative-branch of the exprssion
;; the order of the arguments are different

;; note that the arguments are evaluated backwards
;; e.g. the evaluation for "(a b c d)" would be like: a;d;c;b
;; so the true difference is that:
;; * in "factorial", "n" gets evaluated before "(factorial (- n 1))"
;;   so there's no need to preserve "env" for "n" (because the function call
;;   will mutate the "env" register)
;;   however, when "(factorial (- n 1))" is called, the argument list
;;   is storing the value of "n", register "argl" has to be preserved
;; * in "factorial-alt", "(factorial (- n 1))" gets evaluated before "n"
;;   since the function call evalutes the first argument, there is no need
;;   to preserve "argl". But when it comes to evaluate "n", "env" has to
;;   be preserved.
;;
;; despite these differences, I don't think either program is more efficent
;; than the other.
