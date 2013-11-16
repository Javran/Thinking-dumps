(load "../common/utils.scm")
(load "../common/test-utils.scm")

; if we need to use a binary tree,
;   we should make sure that every key can be compared
;   between each other.
;   * to implement `lookup`, we first look at the root of this table
;   compare `key` with that of the root, if they are equal,
;   return the corresponding value and it's done.
;   otherwise, if `key` < `root key`, call `lookup` recursively on left tree
;   if `key` > `root key`, call `lookup` recursively on right tree
;   there's another case that we reach a leaf. in this case if key != leaf key,
;   lookup fails, else the corresponding value should be returned
;   * to implement `insert!`, just follow the same way of `lookup`, find an empty leave
;   or find an existing key-value pair (depending on the `key`) and just insert the node

(end-script)
