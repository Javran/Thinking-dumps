(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu_listprim2_patch.scm")

(load "./exercise_5_21_controllers.scm")

(define test-instruction-list
  `( ,@(tree->instruction-list '(1 2 (4 5) (a b . c) d e))
     (assign tree (reg result))
     ,@(cdr count-leaves-r-controller)))

(define test-controller
  `(controller
    ,@(rewrite-instructions* (append list-primitives-rules
                                     stack-manip-rules)
                             test-instruction-list)))

(let ((m (build-and-execute
          test-controller
          '())))
  (out (machine-reg-get m 'result)))

;; TODO: maybe we can remove some handlers like (save ???) and (restore ???)
;; to show that we are indeed replacing them with lower level stuff.

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
