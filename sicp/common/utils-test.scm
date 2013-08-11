(load "./utils.scm")
(load "./test-utils.scm")

(let ((add3 ((curry2 +) 3))
      (mul4 ((curry2 *) 4)))
  (let ((testcases
          (list (mat ((compose add3 mul4) 2) 11)
                (mat ((compose mul4 add3) 2) 20))))
    (do-test identity testcases)))
