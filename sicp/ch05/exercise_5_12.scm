(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")

;; TODO: the modification will be based on
;; "simu.scm" but later dependencies will be minimized
;; so that something can be shared between different simulators

(load "./figure_5_12.scm")
(load "./exercise_5_12_analyze.scm")

(pretty-print-data-path-analysis
 (data-path-analyze
  (cdr
   fib-machine-controller)))

(end-script)
