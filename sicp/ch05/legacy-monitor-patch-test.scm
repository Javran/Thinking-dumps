(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./legacy-easy.scm")
(load "./legacy-monitor-patch.scm")

(load "./figure_5_12.scm")

(let ((m (make-and-execute
          `(,@fib-machine-controller
            ;; adding an instruction to show statistics
            ;; at the end of the execution
            (perform (op print-stack-statistics)))
          '((n 5)))))
  'done)
