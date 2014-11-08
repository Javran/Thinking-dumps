(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu_listprim2_patch.scm")

(load "./exercise_5_21_controllers.scm")

(define test-controller
  `(controller
    ,@(tree->instruction-list '(1 2 (4 5) (a b . c) d e))
    (assign tree (reg result))
    ,@(cdr count-leaves-r-controller)))

(let ((m (build-and-execute
          test-controller
          '())))
  (out (machine-reg-get m 'result)))

;; TODO: maybe we can remove some handlers like (save ???) and (restore ???)
;; to show that we are indeed replacing them with lower level stuff.

;; TODO: our target is to keep some compatibility between list-prim and list-prim2
;; maybe we'll eventually rename list-prim into list-prim-fast so that the program
;; works on scheme list primitives, and rename list-prim2 to list-prim
;; so we have a slower implementation but relies less on scheme primitives

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
