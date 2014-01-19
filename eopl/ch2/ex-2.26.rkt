#lang eopl

(require "../common.rkt")
(require "../test-utils.rkt")

(define-datatype red-blue-subtree red-blue-subtree?
  (red-node
    (subtree1 red-blue-subtree?)
    (subtree2 red-blue-subtree?))
  (blue-node
    (subtrees (list-of red-blue-subtree?)))
  (leaf-node
    (num integer?)))

(define-datatype red-blue-tree red-blue-tree?
  (a-red-blue-tree
    (tree red-blue-subtree?)))

(define (mark-red-depth tree)
  ; mark red depth, with a given red node count
  (define (mark-red-depth-aux current-count subtree)
    (cases red-blue-subtree subtree
      (red-node (t1 t2)
        (red-node
          (mark-red-depth-aux (+ current-count 1) t1)
          (mark-red-depth-aux (+ current-count 1) t2)))
      (blue-node (ts)
        (blue-node
          (map ((curry2 mark-red-depth-aux) current-count)
               ts)))
      (leaf-node (n)
        (leaf-node current-count))))
  (cases red-blue-tree tree
    (a-red-blue-tree (subtree)
      (a-red-blue-tree
        (mark-red-depth-aux 0 subtree)))))

(define (red-blue-subtree->string stree)
  (cases red-blue-subtree stree
    (red-node (t1 t2)
      (format "(R ~A ~A)"
        (red-blue-subtree->string t1)
        (red-blue-subtree->string t2)))
    (blue-node (subtrees)
      (format "(B~A)"
        (my-foldl
          (lambda (acc i)
            (format "~A ~A" acc i))
          ""
          (map red-blue-subtree->string subtrees))))
    (leaf-node (n)
      (number->string n))))

(define (red-blue-tree->string tree)
  (cases red-blue-tree tree
    (a-red-blue-tree (subtree)
      (red-blue-subtree->string subtree))))

; make a tree for test:
; (R (B (B 1 2 3) (R (R 4 (B)) (B 5 6)) 7 (B)) 8)
; the result should be:
; (R (B (B 1 1 1) (R (R 3 (B)) (B 2 2)) 1 (B)) 1)

(define test-tree
  (a-red-blue-tree
    (red-node
      (blue-node
        (list
          (blue-node
            (list
              (leaf-node 1)
              (leaf-node 2)
              (leaf-node 3)))
          (red-node
            (red-node
              (leaf-node 4)
              (blue-node '()))
            (blue-node
              (list
                (leaf-node 5)
                (leaf-node 6))))
          (leaf-node 7)
          (blue-node '())))
      (leaf-node 8))))

(out (red-blue-tree->string test-tree)
     (red-blue-tree->string (mark-red-depth test-tree)))
