(load "./exercise_2_96_setup.scm")

(load "./exercise_2_97_polynominal_package.scm")
(load "./exercise_2_97_poly_termlist_sparse_package.scm")

(install-polynomial-package)
(install-poly-termlist-sparse-package)

(run-test 'polynominal-package)
(run-test 'poly-termlist-sparse-package)

(let ()
  ; a dummy scope not to contaminate global env
  (define (reduce-scheme-number n d)
    (assert (integer? n))
    (assert (integer? d))
    (let ((g (gcd n d)))
      (map ((curry2 attach-tag) 'scheme-number)
           (list (quotient n g)
                 (quotient d g)))))
  (put 'reduce '(scheme-number scheme-number) reduce-scheme-number)
  'done)

(define (reduce n d)
  (apply-generic 'reduce n d))

(define (make-rational-g n d)
  (let ((reduced (reduce n d)))
    (let ((nn (car reduced))
          (dd (cadr reduced)))
      ((if (is-poly? nn) make-rational-p make-rational) nn dd))))
