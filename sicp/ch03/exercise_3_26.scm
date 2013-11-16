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

(define (make-node key val)
  (list key val nil nil))

(define (key-node n)   (list-ref n 0))
(define (val-node n)   (list-ref n 1))
(define (ltree-node n) (list-ref n 2))
(define (rtree-node n) (list-ref n 3))

(define (set-key-node! n k)
  (set-car! n k))
(define (set-val-node! n v)
  (set-car! (cdr n) v))
(define (set-ltree-node! n l)
  (set-car! (cddr n) l))
(define (set-rtree-node! n r)
  (set-car! (cdddr n) r))

(define n (make-node 1 2))



(end-script)
