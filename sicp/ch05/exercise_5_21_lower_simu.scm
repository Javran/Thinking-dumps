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

(define (tree->instruction-list data)
  (if (pair? data)
      (let ((il-car (tree->instruction-list (car data)))
            (il-cdr (tree->instruction-list (cdr data))))
        (append il-car
                '((save result))
                il-cdr
                '((save result)
                  (restore cdr-v)
                  (restore car-v)
                  (assign result (op cons) (reg car-v)
                          (reg cdr-v)))))
      `( (assign result (const ,data)) )))

(out "====")
(for-each
 out
 (tree->instruction-list '(1 2 (3 4 . 5))))


(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
