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

(define (make-polynomial var terms) (define (term-pair->termlist tp)
    (let ((t-order (car tp))
          (t-coeff (make-scheme-number
                     (cadr tp))))
      (make-tl-from-args
        'poly-termlist-sparse
        t-order t-coeff)))
  (let ((termlists (map term-pair->termlist terms)))
    (make-poly
      var
      (fold-left
        add
        (make-tl-empty 'poly-termlist-sparse)
        termlists))))

(define (reduce n d)
  (apply-generic 'reduce n d))

(define (make-rational-g n d)
  (let ((reduced (reduce n d)))
    (let ((nn (car reduced))
          (dd (cadr reduced)))
      ((if (is-poly? nn) make-rational-p make-rational) nn dd))))

(let ()
  (define numer car)
  (define denom cdr)

  (define (bin-op-modifier op)
    (lambda (a b)
      (let ((result (contents (op a b))))
        (let ((n (numer result))
              (d (denom result)))
          (make-rational-g n d)))))

  (define origin-add (get 'add '(rational-p rational-p)))
  (define origin-sub (get 'sub '(rational-p rational-p)))
  (define origin-mul (get 'mul '(rational-p rational-p)))
  (put 'add '(rational-p rational-p) (bin-op-modifier origin-add))
  (put 'sub '(rational-p rational-p) (bin-op-modifier origin-sub))
  (put 'mul '(rational-p rational-p) (bin-op-modifier origin-mul))

  'done)
