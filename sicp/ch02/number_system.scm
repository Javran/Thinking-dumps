(load "../common/utils.scm")
(load "../common/test-utils.scm")
(load "./tag_system.scm")


(load "./number_system_scheme_number.scm")
(load "./number_system_rational.scm")

(define eps 1e-9)

(install-scheme-number-package)
(install-rational-package)

(load "./number_system_rect.scm")
(install-rect-package)

(define (run-test tag) ((get 'test tag)))
(define (run-all-test)
  (for-each run-test
            (list
              'tag-system
              'scheme-number-package
              'rational-package
              'rect-package
              ))
  (out "all tests are done."))

(run-all-test)

(end-script)
