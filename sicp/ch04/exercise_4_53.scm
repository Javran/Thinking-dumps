(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")
(load "./exercise_4_51_common.scm")
(load "./exercise_4_52_common.scm")

(define prime-sum-pair-prog
  `(define (prime-sum-pair list1 list2)
     (let ((a (an-element-of list1))
           (b (an-element-of list2)))
       (require (prime? (+ a b)))
       (list a b))))

(define prime?-prog
  `(define (prime? n)
     (define (smallest-divisor n)
       (define (divides? a b)
         (= (remainder b a) 0))
       (define (square x) (* x x))
       (define (find-divisor n test-divisor)
         (cond ((> (square test-divisor) n) n)
               ((divides? test-divisor n) test-divisor)
               (else (find-divisor n (+ test-divisor 1)))))
       (find-divisor n 2))
     (= n (smallest-divisor n))))

(install-amb-if-fail)
(install-amb-permanent-set!)

(run-all-slot-tests)

(out
 (amb-eval-all
  `(begin
     (define (require b)
       (if b 'pass (amb)))
     (define (an-element-of items)
       (require (not (null? items)))
       (amb (car items)
            (an-element-of (cdr items))))

     ,prime-sum-pair-prog
     ,prime?-prog

     (let ((pairs '()))
       (if-fail
        (let ((p (prime-sum-pair '(1 3 5 8)
                                 '(20 35 110))))
          (permanent-set! pairs (cons p pairs))
          (amb))
        pairs)))
  (init-env)))

;; the output should be:
;; (((8 35) (3 110) (3 20)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
