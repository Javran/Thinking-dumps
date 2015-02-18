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

(define test-list '(1 2 3 4))
(list-set! test-list 1 'd)
(list-set! test-list 2 'a)
(list-set! test-list 0 'b)
(list-set! test-list 3 'c)
(assert (equal? test-list '(b d a c)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
