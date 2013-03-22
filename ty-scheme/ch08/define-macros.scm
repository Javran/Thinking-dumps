(load "../common/utils.scm")

(define-syntax-rule (my-when test branch)
  (if test
    (begin branch)))

(my-when #t
  (display "This string should be printed"))

(my-when #f
  (display "This string should not be printed"))



