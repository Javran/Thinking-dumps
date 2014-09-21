(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu-monitor-patch.scm")

(let ((st (empty-stack)))
  (stack-push! st 1)
  (stack-push! st 2)
  (stack-push! st 3)
  (for-each
   (lambda (k)
     (out (stack-meta-get st k)))
   '(number-pushes max-depth current-depth)))
