;; in the original simu.scm,
;; we can store anything we like in a register
;; and don't pay much attention when it comes to
;; pairs and storage.
;; this patch puts the machine to a "lower" level:
;; we now only keep value of basic types in a register
;; and on the stack, pairs will be represented in memory
;; and as "pointers".
