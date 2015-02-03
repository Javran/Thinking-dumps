(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./compiler.scm")

(compiler-insn-seq-tests)
(newline)

;; the correctness of the compiler is further verified using
;; any valid machine implementation.
;; because the compiler shouldn't have knowledge
;; about machines on its own.
;; one of the possible implementation is "simu_compiler_patch.scm"
;; run testcases in "simu_compiler_patch_tests.scm" for further
;; verification.
