(load "../common/utils.scm")

(define t1 '(1 3 (5 7) 9))
(define t2 '((7)))
(define t3 '(1 (2 (3 (4 (5 (6 7)))))))

(out t1 t2 t3)

(define (test-pick-up-7 f ls)
  (if (= (f ls) 7)
    (out "test passed")
    (out "test failed")))

(test-pick-up-7
  (lambda (x)
    (cadr
    (caddr x))) ; (5 7)
  t1)

(test-pick-up-7
  caar
  t2)

(test-pick-up-7
  (lambda (x)
    (cadadr 
      (cadadr 
        (cadadr x))))
  t3)

(end-script)
