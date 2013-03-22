(load "../common/utils.scm")

(define-syntax-rule (my-when test branch)
  (if test
    (begin branch)))

(my-when #t
  (display "This string should be printed"))

(my-when #f
  (display "This string should not be printed"))

(newline)

(define-syntax-rule (my-unless test branch)
  (my-when (not test) branch))

(my-unless #t
  (display "This should not be printed"))

(my-unless #f
  (display "This should be printed"))

(newline)
