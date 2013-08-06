(load "../common/utils.scm")
(load "../common/test-utils.scm")
(load "./tag_system.scm")

(load "./number_system_scheme_number.scm")
(load "./number_system_rational.scm")
(load "./number_system_complex.scm")

(define eps 1e-9)

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(define (run-test tag) ((get 'test tag)))
(define (run-all-test)
  (for-each run-test
            (list
              'tag-system
              'scheme-number-package
              'rational-package
              'complex-package
              ))
  (out "all tests are done."))

(run-all-test)

(end-script)
