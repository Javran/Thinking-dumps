;;; explicit control evaluator with compiled-code supports
;;; see document in "ec-plus.md" for more details

;; this module assume the following two modules
;; has been imported:
;; * simu.scm        for machine implementation
;; * compiler.scm    for compilation supports

(load "ec-plus-machine.scm")
(load "ec-plus-compiler.scm")
