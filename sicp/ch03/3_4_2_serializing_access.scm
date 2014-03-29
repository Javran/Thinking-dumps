(load "../common/utils.scm")
(load "../common/test-utils.scm")

; high level idea:
;   constraint some special procedures that we check and make sure
;     none of the procedures lying in this special category are allowed
;     to run simultaneously.
;   for other procedures, we assume that allowing them to run concurrently
;     will make no mistake.
;   e.g. we put some accessing and modification operations into the same
;     procedure and make sure this procedure cannot run concurrently.

(end-script)
