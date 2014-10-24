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

(define (count-leaves-machine tree)
  ;; evaluate the function by running the machine
  (let ((m (build-and-execute
            count-leaves-controller
            `((tree ,tree)))))
    (machine-reg-get m 'result)))

(for-each
 (lambda (t)
   ;; count-leaves and count-leaves-machine
   ;; should have the same output given the same input
   (assert (= (count-leaves t)
              (count-leaves-machine t))))
 (list
  '()
  'a
  '(1 2 3 (4 5 . 6) 7 ((8 a b d)))))

(out "tests done")

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
