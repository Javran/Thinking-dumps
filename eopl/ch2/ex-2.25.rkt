#lang eopl

(require "../common.rkt")
(require "../test-utils.rkt")

(define-datatype bintree bintree?
  (leaf-node
    (num integer?))
  (interior-node
    (key symbol?)
    (left bintree?)
    (right bintree?)))

(define (leaf-node? tree)
  (cases bintree tree
    (leaf-node (n)
      #t)
    (interior-node (k l r)
      #f)))

(define (max-interior tree)
  ; steps:
  ; #1: flatten the tree, calculate leaf-sum on the way
  ; #2: filter out leaves
  ; #3: return the maximum

  ; flatten the tree to a list, where
  ;   ancesters come prior to its children
  ;   and every node are converted to a pair < k, v >
  ;   for leaves, `k` = #f, for other nodes, `k` = key
  ;   and `v` is always the leaf-sum
  (define (flatten-tree tree)
    (cases bintree tree
      (leaf-node (n)
        ; leaf node, leaf-sum = n
        (list (cons #f n)))
      (interior-node (k l r)
        (let ((l-flatten (flatten-tree l))
              (r-flatten (flatten-tree r)))
          ; since the first element is alway the current root
          (let ((l-sum (cdar l-flatten))
                (r-sum (cdar r-flatten)))
            (append (list (cons k (+ l-sum r-sum)))
                    l-flatten
                    r-flatten))))))
  (define valid-nodes
    ; valid nodes have the key
    (filter car (flatten-tree tree)))
  (assert (not (null? valid-nodes))
    "called with a leaf")
  ; pick up the maximum, remove the value part
  (car
    (my-foldl
      (lambda (max-pair cur-pair)
        (if (> (cdr cur-pair) (cdr max-pair))
          cur-pair
          max-pair))
      (car valid-nodes)
      (cdr valid-nodes))))

(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))

(define tree-2
  (interior-node 'bar (leaf-node -1) tree-1))

(define tree-3
  (interior-node 'baz tree-2 (leaf-node 1)))

(out (max-interior tree-2)
     (max-interior tree-3))
