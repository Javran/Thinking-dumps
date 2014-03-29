(load "../common/utils.scm")
(load "../common/test-utils.scm")

; deadlock:
;   attempts to get some resources that
;   has already taken from other sleeping threads

; give each account an unique number
;   so they can have some priorities over
;   each other.
; I think this is exactly what I'm doing
;   in "./bank_account_exchange.rkt"

(end-script)
