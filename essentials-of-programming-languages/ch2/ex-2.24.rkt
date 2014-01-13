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

; bintree-to-list: Bintree -> List
(define (bintree-to-list tree)
  (cases bintree tree
    (leaf-node (n)
      (list 'leaf-node n))
    (interior-node (k l r)
      (list 'interior-node
            k
            (bintree-to-list l)
            (bintree-to-list r)))))

(out (bintree-to-list
       (interior-node 'a
         (leaf-node 3)
         (leaf-node 4))))
