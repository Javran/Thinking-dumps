(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./exercise_5_16_simu_tracing_patch.scm")
(load "./simu_listprim_patch.scm")

(load "./exercise_5_21_controllers.scm")

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(out (count-leaves '(1 2 3 (4 5 . 6) 7 ((8)))))

(let ((m (build-with
          count-leaves-controller
          '((tree  (1 2 3 (4 5 . 6) 7 ((8)))))
          default-ops-builder)))
  (machine-trace-on! m)
  (machine-fresh-start! m)
  (out (machine-reg-get m 'result)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
