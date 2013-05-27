(load "../common/utils.scm")

; codes are mainly copied from ex 2.30

(define (square-tree-1 tree)
  (map (lambda (sub-tree)
         (if (list? sub-tree)
           (square-tree-1 sub-tree)
           (square sub-tree)))
       tree))

(define (square-tree-2 tree)
  (if (list? tree)
    (if (null? tree)
      nil
      (cons (square-tree-2 (car tree))
            (square-tree-2 (cdr tree))))
    (square tree)))

(define (tree-map f tree)
  ; a tree can be regarded as a list with all sub-trees
  (if (list? tree)
    (map (lambda (sub-tree) (tree-map f sub-tree))
         tree)
    (f tree)))

(define (square-tree-3 tree)
  (tree-map square tree))

(define x
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(out x
     ; (1 (2 (3 4) 5) (6 7))
     (square-tree-1 x)
     (square-tree-2 x)
     (square-tree-3 x)
     ; (1 (4 (9 16) 25) (36 49)) ; 3 times
     )

(end-script)
