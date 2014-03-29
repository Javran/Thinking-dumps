(load "../common/utils.scm")
(load "../common/test-utils.scm")

; restriction version 1:
; no two operations that change any shared state variables
; can occur at the same time.
; (inefficient and overly conservative.)

; restriction version 2:
; a concurrent system produces the same result as if
; the processes had run sequentially in some order.

; restriction version x:
; there are still weaker requirements.

(end-script)
