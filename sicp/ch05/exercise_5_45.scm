;;; these codes are just for running the machine
;;; to get some measurements.
;;; for the results, see "exercise_5_45.odt" for data
;;; and "exercise_5_45.md" for answers.
(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "compiler.scm")
(load "ec-plus.scm")

(define fac-expr
  `(define (factorial n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1))))))

#;
(compile-and-go
 `(begin
    ;; recursive factorial function
    ;;   compiled for collecting stack statistics
    ,fac-expr
    ))

;; print the instruction sequence and learn the difference.
(print-instruction-sequence
 (compile-and-check `(begin
                       ,fac-expr)))

;; reloading the simulator and run the special purpose version
;; factorial machine
(load "simu.scm")
(load "simu-monitor-patch.scm")
(load "figure_5_11.scm")

(let ((m (ctl-ops->machine
          fac-machine-controller
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
   '(1 2 10 20 50 100 1000)))
