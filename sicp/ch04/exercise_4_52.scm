(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")

;; still not sure if my solution is the true intention of
;; exercise 4.52, but here my solution does match the output
;; given in the exercise.
;; The weird thing is: in the exercise, if the list does not
;; contain any odd numbers, "all-odd" is print, and if the list
;; has one even number, what will happen after that even number
;; get printed? "all-odd" sounds not good literally.

(load "./exercise_4_52_common.scm")

(install-amb-if-fail)

(run-all-slot-tests)

(define (test-prog l)
  `(if-fail (let ((x (amb ,@l)))
              (define (require x)
                (if x 'pass (amb)))
              (require (even? x))
              x)
            'all-odd))

(out (amb-eval-all (test-prog '(1 3 5)) (init-env)))
(out (amb-eval-all (test-prog '(1 3 5 8)) (init-env)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
