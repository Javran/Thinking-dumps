(load "../common/utils.scm")

; "nine cases" I think too bad to call it a hint.
; anyway ... I guess its 3 cases x 3 cases, now let's figure it out:
; inv -> interval
; lb -> lower-bound
; ub -> upper-bound

; case #1: inv 1 <= 0, inv 2 <= 0
; [inv 1]
; ----------0---------->
; [inv 2]
; result: [ub1*ub2 - lb1*lb2]

; case #2: inv 1 <= 0, inv 2 spans 0
; [inv 1]
; ----------0---------->
;        [inv 2]
; result: [lb1*ub2 - lb1*lb2]

; case #3: inv 1 < 0, inv 2 > 0
; [inv 1]
; ----------0---------->
;                [inv 2]
; result: [lb1*ub2 - ub1*lb2]

; case #4:
;        [inv 1]
; ----------0---------->
; [inv 2]
; result: [ub1*lb2 - lb1*lb2]

; case #5:
;        [inv 1]
; ----------0---------->
;        [inv 2]
; result: [min(lb1*ub2,ub1*lb2) - max(lb1*lb2,ub1*ub2)]

; case #6:
;        [inv 1]
; ----------0---------->
;                [inv 2]
; result: [lb1*ub2 - ub1*ub2] 

; case #7:
;                [inv 1]
; ----------0---------->
; [inv 2]
; result: [lb1*ub2 - ub1*lb2]

; case #8:
;                [inv 1]
; ----------0---------->
;        [inv 2]
; result: [ub1*lb2 - ub1*ub2]

; case #9:
;                [inv 1]
; ----------0---------->
;                [inv 2]
; result: [lb1*lb2 - ub1*ub2]
