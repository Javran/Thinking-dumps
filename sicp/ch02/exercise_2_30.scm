(load "../common/utils.scm")

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

(define x
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(out x
     ; (1 (2 (3 4) 5) (6 7))
     (square-tree-1 x)
     (square-tree-2 x)
     ; (1 (4 (9 16) 25) (36 49))
     )

(end-script)
