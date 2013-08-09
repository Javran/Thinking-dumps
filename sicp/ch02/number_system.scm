; required packages:
; (load "../common/utils.scm")
; (load "../common/test-utils.scm")
; (load "./tag_system.scm")

(load "./number_system_rational.scm")
(load "./number_system_scheme_number.scm")
(load "./number_system_complex.scm")
(load "./number_system_native_num_test.scm")
(load "./number_system_coercion.scm")

(define eps 1e-9)

(install-rational-package)
(install-scheme-number-package)
(install-complex-package)
(install-native-num-test)
(install-coercion-test)

(define (run-test tag) ((get 'test tag)))
(define (run-all-test)
  (for-each run-test
            (list
              'tag-system
              'scheme-number-package
              'rational-package
              'complex-package
              'native-num
              'coercion-system
              ))
  (out "all tests are done."))
