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

(let ((x1 (make-tl-from-args 'poly-termlist-sparse
            ; x^5 - 1
            5 (make-scheme-number 1)
            0 (make-scheme-number -1)))
      (x2 (make-tl-from-args 'poly-termlist-dense
            ; x^2 - 1
            2 (make-scheme-number 1)
            0 (make-scheme-number -1)))
      )
  (let ((p1 (make-poly 'x x1))
        (p2 (make-poly 'x x2)))
    (let* ((result (div p1 p2))
           (q (car result))
           (r (cadr result)))
      (out "divisor:" (to-string p1)
           "dividend:" (to-string p2)
           "quotient:" (to-string q)
           "remainder:" (to-string r))
      )))

(let ((tl1 (make-tl-from-cseq-num
             'poly-termlist-sparse
             9 -6 -4 -2 1 2 4))
      (tl2 (make-tl-from-cseq-num
             'poly-termlist-dense
             3 2 1)))
  (let ((p1 (make-poly 'x tl1))
        (p2 (make-poly 'x tl2)))
    (let* ((result (div p1 p2))
           (q (car result))
           (r (cadr result)))
      (out "divisor:" (to-string p1)
           "dividend:" (to-string p2)
           "quotient:" (to-string q)
           "remainder:" (to-string r))
      )))

(let ((p1 (make-poly 'wildcard
                     (make-tl-from-cseq-num
                       'poly-termlist-sparse
                       1 2 3 4 5)))
      (p2 (make-poly 'x
                     (make-tl-from-cseq-num
                       'poly-termlist-sparse
                       1 2 3 4 5))))
  (out (mul p1 p2))
  (out (mul p1 (make-scheme-number 10)))
  (out (mul p2 (make-scheme-number 10))))

; TODO:
; * test if now we can enable something like:
;   (y^2+y+1) x^3 + 2 x^3
;   where `(y^2+y+1) + 2` becomes possible


(end-script)
