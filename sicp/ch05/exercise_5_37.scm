(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./compiler.scm")
(load "./simu.scm")
(load "./simu_compiler_patch.scm")

(define simple-exp
  '(* 2 3 4))

(define insn-seq-before
  (compile-and-check simple-exp))

(load "./exercise_5_37_compiler.scm")

(load "./ec-tests.scm")
(load "./exercise_5_23_tests.scm")
(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)
(newline)

(define insn-seq-after
  (compile-and-check simple-exp))

(out ";;;; before")
(for-each out (statements insn-seq-before))
(out ";;;; after")
(for-each out (statements insn-seq-after))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
