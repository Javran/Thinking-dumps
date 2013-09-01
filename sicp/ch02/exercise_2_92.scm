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

(let ((args1 (list 
               1 (make-scheme-number 2)
               3 (make-scheme-number 4)))
      (args2 (list 
               3 (make-scheme-number 2)
               2 (make-scheme-number -2)
               0 (make-scheme-number -2)))
      (types (list 'poly-termlist-sparse
                   'poly-termlist-dense)))
  (let loop ((t types))
    (if (null? t)
      'done
      (let ((p1 (make-poly
                  'x
                  (apply make-tl-from-args
                         (cons (car t) args1))))
            (p2 (make-poly
                  'x
                  (apply make-tl-from-args
                         (cons (car t) args2)))))
        (format #t "impl ~A:~%" (car t))
        (out (to-string (sub p1 p2)))
        (loop (cdr t)))))
  (newline)
  (out 
    (to-order-coeff-list
      (apply make-tl-from-args
             (cons 'poly-termlist-sparse
                   args1)))
    (to-order-coeff-list
      (apply make-tl-from-args
             (cons 'poly-termlist-dense
                   args2))))
  )

(end-script)
