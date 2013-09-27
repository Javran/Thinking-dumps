(load "./tag_system.scm")
(load "./exercise_2_92_number_system.scm")
(load "./exercise_2_92_setup.scm")

(load "./exercise_2_93_number_system_rational_p.scm")

(install-rational-p-package)

(define make-rational-p 
  (get 'make 'rational-p))

(define (make-polynomial var terms)
  (define (term-pair->termlist tp)
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
