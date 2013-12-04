; #lang racket
; this module is supposed to be loaded rather than
;   be executed directly.

(define (parallel-execute . thunks)
  (map thread thunks))
