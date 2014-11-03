(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu_lower_patch.scm")
(load "./rewrite.scm")
(load "./simu_listprim2_patch.scm")

(define simple-instructions
  '((assign a (op cons) (const 2) (const ()))
    (assign b (op cons) (const 1) (reg a))
    (assign c (op cons) (const 0) (reg b))
    (assign d (reg c))))

(for-each out simple-instructions)
(newline)

(for-each out (rewrite-instructions
               list-primitives-rules
               simple-instructions))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
