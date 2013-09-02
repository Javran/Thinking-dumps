(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./tag_system.scm")
(load "./number_system.scm")

; let's get a fresh start, summarize what we've done
;   before this exercise and organize codes together
;   for the incoming task
(load "./exercise_2_92_setup.scm")

(define (make-poly v ls)
  ((get 'make 'polynominal) v ls))

(define variable ((curry2 apply-generic) 'variable))
(define term-list ((curry2 apply-generic) 'term-list))

; demo for automatic termlist type conversion
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

(end-script)
