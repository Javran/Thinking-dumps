(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./exercise_5_19_simu_breakpoint_patch.scm")

(load "./figure_5_12.scm")

(let ((m (empty-machine)))
  (machine-set-breakpoint! m 'lbla 10)
  (machine-set-breakpoint! m 'lbla 2)
  (machine-set-breakpoint! m 'lblb 4)
  (machine-set-breakpoint! m 'lbla 1)
  (machine-set-breakpoint! m 'lbla 1)

  (out
   (machine-breakpoint? m 'lbla 10)     ; #t
   (machine-breakpoint? m 'lblb 2)      ; #f
   (machine-breakpoint? m 'lbla 2)      ; #t
   )

  (machine-cancel-breakpoint! m 'lbla 2)
  (machine-cancel-breakpoint! m 'lbla 3)
  (machine-cancel-breakpoint! m 'lblb 4)

  (out
   (machine-breakpoint? m 'lbla 10)     ; #t
   (machine-breakpoint? m 'lblb 2)      ; #f
   (machine-breakpoint? m 'lbla 2)      ; #f
   )

  (machine-cancel-all-breakpoints! m)

  (out
   (machine-breakpoint? m 'lbla 10)     ; #f
   (machine-breakpoint? m 'lblb 2)      ; #f
   (machine-breakpoint? m 'lbla 2)      ; #f
   )

  'done)


(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
