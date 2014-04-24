(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")

(load "./exercise_4_51_common.scm")

(install-amb-permanent-set!)
(run-all-slot-tests)

(define (run-test-with symb-set)
  (out
   (amb-eval-all
    `(begin
       (define (not x)
         (if x #f #t))
       (define (require x)
         (if x 'pass (amb)))
       (define count 0)
       (let ((x (amb 'a 'b 'c))
             (y (amb 'a 'b 'c)))
         (,symb-set count (+ count 1))
         (require (not (eq? x y)))
         (list x y count)))
    (init-env))))

;; permanent-set! cannot be undone,
;; therefore "count" will keep counting
(run-test-with `permanent-set!)
;; however for "set!" every time before
;; a new candidate is about to be attempted,
;; the previous effect caused by "set!" is cancelled
;; causing "count" to stay at 1 and never get a chance
;; of increasing further.
(run-test-with `set!)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
