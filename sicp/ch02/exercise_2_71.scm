(load "../common/utils.scm")

; relative frequency: 1, 2, 4, 8 ...
; for n = 5, the tree is like:
; ((((a:1 b:2):3 c:4):7 d:8):15 e:16):31
; for n = 10, the tree is like:
; ((((( ((((a:1 b:2):3 c:4):7 d:8):15 e:16):31
;   f:32):63 g:64):127 h:128):255 i:256):511 j:512):1023
; the most frequent symbol needs only 1 bit to encode
; whereas the least frequent symbol needs (n-1) bits to encode

(end-script)
