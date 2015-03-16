;; "+" and "*" can accept arbitrary numbers
;; of operands because (number,+) and (number,*) are commutative monoids
;; it doesn't matter how they gets combined together.
;; rather than taking care of register handlings, there is a simple way:
;; * enforce "spread-arguments" to deal with two-argument cases only
;; * do syntactic transformation before compilation (i.e. "transform-right")
;;   e.g.
;;   (+ 1)         => 1
;;   (+ 1 2)       => (+ 1 2)
;;   (+ 1 2 3)     => (+ 1 (+ 2 3))
;;   (+ 1 2 3 4)   => (+ 1 (+ 2 (+ 3 4)))
;;   ...
;; * however, the side effects incurred by evaluating arguments
;;   is not guaranteed to happen in a certain order, since
;;   different compilers can make different decisions about
;;   the argument evaluation order. here, in order to keep
;;   consistent with the original implementation,
;;   we choose to "fold" from right to left
;;   therefore arguements will still be evaluated from right to left

(define (transform-right exp exp-zero)
  (let ((operator (car exp))
        (operands (cdr exp)))
    (reduce-right
     (lambda (i acc)
       (list operator i acc))
     exp-zero
     operands)))

;; a table of transformable function calls
;; whose elements are "(list <op-symbol> <exp-zero>)"s
(define transformable-table
  `((+ 0)
    (* 1)))

;; Local variables:
;; proc-entry: "./exercise_5_38_tests.scm"
;; End:
