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
  (let ((ltree nil)
        (rtree nil))
    (define (set-key! x) (set! key x) 'ok)
    (define (set-val! x) (set! val x) 'ok)
    (define (set-ltree! x) (set! ltree x) 'ok)
    (define (set-rtree! x) (set! rtree x) 'ok)

    (define (dispatch m)
      (cond ((eq? m 'key) key)
            ((eq? m 'val) val)
            ((eq? m 'set-key!) set-key!)
            ((eq? m 'set-val!) set-val!)
            ((eq? m 'ltree) ltree)
            ((eq? m 'rtree) rtree)
            ((eq? m 'set-ltree!) set-ltree!)
            ((eq? m 'set-rtree!) set-rtree!)
            ))
    dispatch))

(end-script)
