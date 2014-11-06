(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu_lower_patch.scm")
(load "./rewrite.scm")
(load "./simu_listprim2_patch.scm")

(load "./exercise_5_21_controllers.scm")

;; TODO: need to construct input data (trees)
;; in such a manner as well..

;; TODO: I think it is possible that we
;; turn a tree under scheme into an instruction list that builds it ...

(for-each out
          (rewrite-instructions* (append list-primitives-rules
                                         stack-manip-rules)
                                 (cdr count-leaves-r-controller)))



(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
