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

(let ((p1 (make-poly
            'x
            (make-tl-from-args
              'poly-termlist-sparse
              1 (make-scheme-number 2)
              3 (make-scheme-number 4))))
      (p2 (make-poly
            'x
            (make-tl-from-args
              'poly-termlist-sparse
              3 (make-scheme-number 2)
              2 (make-scheme-number -2)
              0 (make-scheme-number -2)))))
  (out (to-string (sub p1 p2))))

(end-script)
