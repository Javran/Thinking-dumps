(load "../common/utils.scm")
(load "../common/test-utils.scm")

; I make all `primitive` in procedure name to be `prim`
;   to avoid some problem, as mit-scheme has already offered
;   a `primitive-procedure?`, but I think that one is different
;   from ours.

; here we can just use the native `apply`.
;   but there might be some difference to switch
;   between our evaluator and the native scheme evaluator
;   here I just mark it as "not implemented",
;   and will come back later
(define (apply-prim-procedure proc args)
  (error "not implemented"))

; we can make a list of procedures
;   that we think proper to be exposed to the
;   implemented language.
;   leave it unimplemented.
(define (prim-procedure? proc)
  (error "not implemented"))

(end-script)
