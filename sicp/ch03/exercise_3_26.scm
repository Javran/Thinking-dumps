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

(load "./exercise_3_26_node.scm")

(define (make-table)
  (cons '*table-bintree* nil))

(define (lookup key table)
  (define (lookup-intern key cmp table)
    (if (null? table)
      #f
      (let ((result (cmp key (key-node table))))
        (cond ((< result 0) (lookup-intern key cmp (ltree-node table)))
              ((> result 0) (lookup-intern key cmp (rtree-node table)))
              ((= result 0) (val-node table))))))
  (lookup-intern key - (cdr table)))

(define (insert! key val table)
  ; mutator! is called when `insert-intern!` knows
  ;   what is the value needed to insert in this place
  ;   but doesn't know how to insert this value
  (define (insert-intern! key val cmp table mutator!)
    (if (null? table)
      ; this node should be filled with a node,
      ;   don't know how to do it, call mutator for help
      (mutator! (make-node key val))
      (let ((result (cmp key (key-node table))))
        (cond ((< result 0)
                (insert-intern!
                  key val cmp
                  (ltree-node table)
                  ; tell the callee, call this lambda
                  ;   if you know what it should be
                  ;   but don't know where it should be
                  (lambda (new-ltree)
                    (set-ltree-node! table new-ltree))))
              ((> result 0)
                (insert-intern!
                  key val cmp
                  (rtree-node table)
                  (lambda (new-rtree)
                    (set-rtree-node! table new-rtree))))
              ((= result 0) (set-val! table val))))))
  (insert-intern!
    key val - (cdr table)
    ; tell the callee how to manipulate the structure
    (lambda (new-table) (set-cdr! table new-table))))

(define (table->string t)
  (define (table->string-intern t)
    (if (null? t)
      "nil"
      (format #f "(k:~A v:~A l:~A r:~A)"
              (key-node t)
              (val-node t)
              (table->string-intern (ltree-node t))
              (table->string-intern (rtree-node t)))))
  (table->string-intern (cdr t)))

(define n (make-table))

(insert! 4 'a n)
(insert! 2 'b n)
(insert! 6 'c n)
(insert! 1 'd n)
(insert! 3 'e n)
(insert! 5 'f n)
(insert! 7 'g n)

(out (table->string n))
; dump table to string and then output

(out (map ((curry2 (flip lookup)) n)
          (list-in-range 1 7)))
; d b e a f c g

(end-script)
