(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; Using approach 1
;; (see exercise_5_14.md for explanation)

(load "simu.scm")
(load "simu-monitor-patch.scm")

(load "figure_5_11.scm")

(let ((m (ctl->machine
          fac-machine-controller)))
  (let loop ((n 2))
    (if (<= n 10)
        (begin
          (machine-init-regs! m `((n ,n)))
          (machine-fresh-start! m)
          (let ((stat (stack-get-statistics (machine-stack m))))
            (format
             #t
             "n = ~A, number-pushes = ~A, max-depth = ~A~%"
             n
             (cdr (assoc 'number-pushes stat))
             (cdr (assoc 'max-depth stat))))
          (loop (add1 n)))
        'done)))

;; Using approach 2
(define controller-text
  `(controller
    (assign maxn (const 10))
    ex-5-14-start
    ;; if n > maxn then end else keep going
    (test (op >) (reg n) (reg maxn))
    (branch (label ex-5-14-end))
    (perform (op printn))
    (perform (op initialize-stack))
    ,@(cdr fac-machine-controller)
    (perform (op print-stack-statistics))
    ;; n = n + 1
    (assign n (op +) (reg n) (const 1))
    (goto (label ex-5-14-start))
    ex-5-14-end))

(build-and-execute-with
 controller-text
 '((n 2))
 (lambda (m)
   `((printn ,(lambda ()
                (format
                 #t "n = ~A~%"
                 (machine-reg-get m 'n))))
     ,@(default-ops-builder m))))

;; see "./exercise_5_14.md" for results

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
