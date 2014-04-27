(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")

(define code-aux
  `(define (an-integer-between a b)
     (define (add1 x) (+ x 1))
     (if (<= a b)
         (amb a (an-integer-between (add1 a) b))
         (amb))))

(define code-1
  `(begin
     ,code-aux
     (define (a-pythagorean-triple-between low high)
       (let ((i (an-integer-between low high)))
         (let ((j (an-integer-between i high)))
           (let ((k (an-integer-between j high)))
             (require (= (+ (* i i) (* j j)) (* k k)))
             (list i j k)))))
     (a-pythagorean-triple-between 1 20)))

(define code-2
  `(begin
     ,code-aux
     (define (a-pythagorean-triple-between low high)
       (let ((i (an-integer-between low high))
             (hsq (* high high)))
         (let ((j (an-integer-between i high)))
           (let ((ksq (+ (* i i) (* j j))))
             (require (>= hsq ksq))
             (let ((k (sqrt ksq)))
               (require (integer? k))
               (list i j k))))))
     (a-pythagorean-triple-between 1 20)))

;; the latter one should be faster

(time-test amb-eval-all code-1 (amb-init-env))
;; 761
;; 767
(time-test amb-eval-all code-2 (amb-init-env))
;; 129
;; 132

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
