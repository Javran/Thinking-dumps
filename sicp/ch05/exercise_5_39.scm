(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")

(load "exercise_5_39_common.scm")

(define test-env
  (extend-environment
   '(a b c d)
   '(a b c d)
   (extend-environment
    '(z y x)
    '(z y x)
    (extend-environment
     '(e f g h)
     '(1 2 3 4)
     the-empty-environment))))

(do-test
 lexical-address-lookup
 (list
  (mat '(0 . 0) test-env 'a)
  (mat '(0 . 3) test-env 'd)
  (mat '(1 . 1) test-env 'y)
  (mat '(1 . 2) test-env 'x)
  (mat '(2 . 3) test-env 4)
  ))

;; tests for list-set!
(define test-list '(1 2 3 4))
(list-set! test-list 1 'd)
(list-set! test-list 2 'a)
(list-set! test-list 0 'b)
(list-set! test-list 3 'c)
(assert (equal? test-list '(b d a c)))

;; tests for lexical-address-set!
(lexical-address-set! '(1 . 2) test-env 'e12)
(lexical-address-set! '(2 . 1) test-env 'e21)
(lexical-address-set! '(0 . 2) test-env 'e02)

;; redo variable lookup
(do-test
 lexical-address-lookup
 (list
  (mat '(0 . 2) test-env 'e02)
  (mat '(1 . 2) test-env 'e12)
  (mat '(2 . 1) test-env 'e21)
  (mat '(1 . 0) test-env 'z)
  (mat '(2 . 3) test-env 4)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
