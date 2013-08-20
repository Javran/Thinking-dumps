(load "./utils.scm")
(load "./test-utils.scm")

(let ((add3 ((curry2 +) 3))
      (mul4 ((curry2 *) 4)))
  (let ((testcases
          (list (mat ((compose add3 mul4) 2) 11)
                (mat ((compose mul4 add3) 2) 20))))
    (do-test identity testcases)))

(let ((a '((1 2 3) (4 5 6) () (7 8 9)))
      (b '((1 2 3) (4 5 6) () (7)))
      (c '((1 2 3 4 5) (6 7 8))))
  (let ((testcases
          (list (mat a b #f)
                (mat a c #f)
                (mat b c #f)
                (mat '() '() #t)
                (mat '() 'a #f)
                (mat '() '(a) #f)
                (mat a a #t)
                (mat b b #t)
                (mat c c #t))))
    (do-test (rec-eq? =) testcases)))

(let ((testcases
        (list (mat 1 2 #t)
              (mat 3 4 #t)
              (mat 1 3 #f)
              (mat 2 4 #f))))
  (do-test (rec-eq? (close-number? 1.5)) testcases))
