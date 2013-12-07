(load "../common/utils.scm")
(load "../common/test-utils.scm")

; as we see in the material and in my exercise 3.38.b
;   it's just inpractical to check every possible event orders
;   and see if there are valid or not.
; for a more practical approach, we should devise general mechanisms
;   that allow us to constrain the interleaving of concurrent processes
;   so that we can be sure that the program behavior is correct

(end-script)
