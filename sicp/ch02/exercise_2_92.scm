(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./tag_system.scm")
(load "./exercise_2_92_number_system.scm")

; let's get a fresh start, summarize what we've done
;   before this exercise and organize codes together
;   for the incoming task
(load "./exercise_2_92_setup.scm")

(define (make-poly v ls)
  ((get 'make 'polynominal) v ls))

(define variable ((curry2 apply-generic) 'variable))
(define term-list ((curry2 apply-generic) 'term-list))

; demo for automatic termlist type conversion
(out "type conversion demo:")
(let ((args1 (list 
               1 (make-scheme-number 2)
               3 (make-scheme-number 4)))
      (args2 (list 
               3 (make-scheme-number 2)
               2 (make-scheme-number -2)
               0 (make-scheme-number -2)))
      (types (list 'poly-termlist-sparse
                   'poly-termlist-dense)))
  (let loop1 ((type1 types))
    (if (null? type1)
      'done
      (begin 
        (let loop2 ((type2 types))
          (if (null? type2)
            'done
            (begin
              (let ((pa1 (make-poly
                           'x
                           (apply
                             make-tl-from-args
                             (cons (car type1)
                                   args1))))
                    (pa2 (make-poly
                           'x
                           (apply
                             make-tl-from-args
                             (cons (car type2)
                                   args2)))))
                (out (sub pa1 pa2)))
              (loop2 (cdr type2)))))
        (loop1 (cdr type1))))))

(out "addition and multiplication demo:")
(let ((make-test-poly
        (lambda (vars extra)
          (let* ((v1 (car vars))
                 (v2 (cadr vars))
                 (v3 (caddr vars))
                 ; p1 =       3 v1^2 + 2 v1^1 + 1 v1^0
                 ; p2 = p1 * (3 v2^2 + 2 v2^1 + 1 v2^0)
                 ; p3 = p2 * (3 v3^2 + 2 v3^1 + 1 v3^0) + extra * v3^1
                 (p1 (make-poly
                       v1
                       (make-tl-from-cseq-num
                         'poly-termlist-sparse
                         3 2 1)))
                 (p2 (make-poly
                       v2
                       (make-tl-from-args
                         'poly-termlist-sparse
                         2 (mul (make-scheme-number 3)
                                p1)
                         1 (mul (make-scheme-number 2)
                                p1)
                         0 (mul (make-scheme-number 1)
                                p1))))
                 (p3 (make-poly
                       v3
                       (make-tl-from-args
                         'poly-termlist-sparse
                         2 (mul (make-scheme-number 3)
                                p2)
                         1 (add (mul (make-scheme-number 2)
                                     p2)
                                (make-scheme-number extra))
                         0 (mul (make-scheme-number 1)
                                p2)))))
            p3))))
  (let ((t1 (make-test-poly '(x y z)  1))
        (t2 (make-test-poly '(x z y)  1))
        (t3 (make-test-poly '(y x z) -1))
        (t4 (make-test-poly '(y z x)  1))
        (t5 (make-test-poly '(z x y) -1))
        (t6 (make-test-poly '(z y x) -1)))
    ; (t1+t2+t3+t4+t5+t6) = 6 * (t1 - z^1) = 6 * (t2 - y^1) = ...
    (out (to-string (simplify (sub (add (add (add t1 t2) t4)
                                        (add (add t3 t5) t6))
                                   (mul (make-scheme-number 6)
                                        (make-test-poly '(x y z) 0))))))
    ; (t1+t2+t4) - (t3+t5+t6) = 2x^1 + 2y^1 + 2z^1
    (out (to-string (simplify (sub (add (add t1 t2) t4)
                                   (add (add t3 t5) t6)))))
    ))

(end-script)
