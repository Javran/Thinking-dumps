(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")

(define an-integer-between-code
  `(define (an-integer-between a b)
    (define (add1 x) (+ x 1))
    (if (<= a b)
        (amb a (an-integer-between (add1 a) b))
        (amb))))

(out (amb-eval-all
      `(begin
         ,an-integer-between-code
         (an-integer-between 1 10))
      (amb-init-env)))
;; output should be: (1 2 3 4 5 6 7 8 9 10)

(out (amb-eval-all
      `(begin
         ,an-integer-between-code
         (define (a-pythagorean-triple-between low high)
           (let ((i (an-integer-between low high)))
             (let ((j (an-integer-between i high)))
               (let ((k (an-integer-between j high)))
                 (require (= (+ (* i i) (* j j)) (* k k)))
                 (list i j k)))))
         (a-pythagorean-triple-between 1 10))
      (amb-init-env)))
;; output should be: ((3 4 5) (6 8 10))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
