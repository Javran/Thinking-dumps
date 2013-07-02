(load "../common/utils.scm")

; to answer what is the order of growth in general is difficult
;   I guess that is because we can not make sure of how the Huffman tree is constructed
; so we consider a special case described in ex 2.71 instead:
;   for the most frequent symbol:
;     takes ~ theta(n) steps to seek the symbol (the most frequent -> the last listed)
;     takes constant steps to reach a leave
;   for the least frequent symbol:
;     takes ~ 1 steps to seek the symbol but takes theta(n) steps to reach a leave

; TODO: seems we can optimize our algorithm by putting more simple structure in the left branch
;   so that it would take only few steps for the most frequent symbol to reach a leaf and get encoded
(end-script)
