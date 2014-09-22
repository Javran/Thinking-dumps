(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu-monitor-patch.scm")

(let ((st (empty-stack)))
  (stack-push! st 1)
  (stack-pop! st)
  (stack-print-statistics st))
