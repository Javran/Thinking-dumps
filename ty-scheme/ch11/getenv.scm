(load "../common/utils.scm")

; the equivalent of 'getenv' is:
(define getenv get-environment-variable)

(define print-env
  (lambda (v)
    (display v)
    (display ": ")
    (out (getenv v))))

(for-each
  print-env
  '("HOME" "PATH" "SHELL"))
