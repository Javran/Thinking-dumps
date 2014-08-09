;; update-insts! will preserve the order of all the labels
;; despite there are multiple labels with exactly the same name,
;; there are all kept in the original implementation.
;; however when looking up the table, the first binding wins.
;; `a` will be 3 when reaching the label `there`


