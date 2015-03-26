;;; primitives for the compiler

;; import primitives from our explicit control evaluator
;; NOTE: the primitives are more than necessary,
;; for example "prompt-for-input" is defined
;; but not actually used in our compiler.
(load "ec-prim.scm")

;; include syntax extensions from ex 5.23
(load "exercise_5_23_common.scm")
