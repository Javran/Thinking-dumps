(load "../common/utils.scm")

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (list? sub-tree)
           (square-tree sub-tree)
           (square sub-tree)))
       tree))

(define x
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(out x
     ; (1 (2 (3 4) 5) (6 7))
     (square-tree x)
     ; (1 (4 (9 16) 25) (36 49))
     )

(end-script)
