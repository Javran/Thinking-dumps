(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./exercise_5_19_simu_breakpoint_patch.scm")

(load "./figure_5_12.scm")

(let* ((k1 '())
       (k2 (breakpoint-table-add 'lbla 10 k1))
       (k3 (breakpoint-table-add 'lbla 2 k2))
       (k4 (breakpoint-table-add 'lblb 4 k3))
       (k5 (breakpoint-table-add 'lbla 1 k4)))
  (out k1 k2 k3 k4 k5))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
