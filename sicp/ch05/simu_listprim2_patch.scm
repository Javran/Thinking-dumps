;; the real implementation
;; of list primitives
;; I guess this will eventually replace the ad hoc
;; implementation of "listprim"

;; usually we rewrite an instruction
;; into a sequence of instructions that does
;; some lower level operations
;; so here a single rule consists of a pattern matching
;; on an instruction and rewriting rule to rewrite it into
;; *a list* of instructions.

(load "./list-stack-rewrites.scm")
(load "./rewrite-instructions.scm")
