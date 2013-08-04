(load "../common/utils.scm")
(load "../common/test-utils.scm")
(load "./tag_system.scm")

(define eps 1e-9)

(load "./number_system_scheme_number.scm")
(install-scheme-number-package)

(define (run-test tag) ((get 'test tag)))
(define (run-all-test)
  (for-each run-test
            (list
              'tag-system
              'scheme-number-package))
  (out "all tests are done."))

(end-script)
