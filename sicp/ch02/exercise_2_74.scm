(load "../common/utils.scm")

; * one record file for each division
; * keyed on employees' names
; * each employees' record is a set (I think it's actually a dict)
;   * according to the requirement, we should at least have these fields:
;     * address
;     * salary

; each record will be stored as an s-expr
;   and put into a single file for each division
;   so that the I/O and file parsing is omitted here to simplify the code
; I'll design 3 different data structures for 3 indenpendent divisions
;   and then implement procedures required to demonstrate the ability of
;   data-directed programming

; furthermore, I assume the name is unique to all the employees represented.
;   so we don't need to care about the name confliction 

(end-script)
