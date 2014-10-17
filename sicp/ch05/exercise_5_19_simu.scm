(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./exercise_5_19_simu_breakpoint_patch.scm")

(load "./figure_5_12.scm")

(let* ((k1 '())
       (k2 (breakpoint-table-add 'lbla 10 k1))
       (k3 (breakpoint-table-add 'lbla 2 k2))
       (k4 (breakpoint-table-add 'lblb 4 k3))
       (k5 (breakpoint-table-add 'lbla 1 k4))
       (k6 (breakpoint-table-del 'lbla 2 k5))
       (k7 (breakpoint-table-del 'lbla 3 k6))
       (k8 (breakpoint-table-del 'lblb 4 k7)))
  (out k1 k2 k3 k4 k5)
  (out "----")
  (out k6 k7 k8))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
