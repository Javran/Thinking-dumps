;;; for details, see "./exercise_5_46.ods"
;;; Number of pushes (compiled:interpreted): close to 0.233.
;;; Maximum stack depth (compiled:interpreted): around 0.7, not stable.
;;; Number of pushes (special-purpose:compiled): around 0.3, not stable.
;;; Maximum stack depth (special-purpose:compiled): around 0.474.

(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "compiler.scm")
(load "ec-plus.scm")

(define fib-expr
  `(define (fib n)
     (if (< n 2)
         n
         (+ (fib (- n 1))
            (fib (- n 2))))))
#;
(compile-and-go
 `(begin
    ,fib-expr))

(load "simu.scm")
(load "simu-monitor-patch.scm")
(load "figure_5_12.scm")

(let ((m (ctl-ops->machine
          fib-machine-controller
          default-ops-builder)))
  (for-each
   (lambda (n)
     (machine-init-regs! m `((n ,n)))
     (machine-fresh-start! m)
     (let ((stat (stack-get-statistics (machine-stack m))))
       (format
        #t
        "n = ~A, number-pushes = ~A, max-depth = ~A~%"
        n
        (cdr (assoc 'number-pushes stat))
        (cdr (assoc 'max-depth stat)))))
   (list-in-range 1 10)))
