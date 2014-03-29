(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./circuit_simulate.scm")

; assume the input signal for an and-gate is (0,1)
; we run (set-signal! in1 1) (set-signal! in2 0)
; 4 procedures will be put on the agenda, in time order:
; * (set-signal! out 1) ; because 1 and 1 = 1
; * (set-signal! out 1) ; same
; * (set-signal! out 0) ; because 1 and 0 = 0
; * (set-signal! out 0) ; same

; if these procedures run in reversed order(LIFO),
;   the final result will be 1, which is incorrect.

; essentially this problem is just like a "resource race" problem
;   that we make two changes "in parallel" (actually we run "add-action!"
;   twice in order, so it's not accurate to say that's in parallel, but the
;   delayed modifications(one put on the agenda) do) and both of them are
;   trying to put their results on the same variable.

(end-script)
