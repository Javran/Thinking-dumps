(load "../common/utils.scm")

(define (scale-tree tree factor)
  (cond ((null? tree)
          nil)
        ((not (list? tree))
          ; is not a list
          (* tree factor))
        (else
          ; is a tree, do things recursively
          (cons (scale-tree (car tree) factor)
                (scale-tree (cdr tree) factor)))))

(define x-1
  (list 1
        (list 2
              3
              (list 4))
        (list 5
              (list)
              (list 6 7 8))
        9
        (list 10)))

(define x-2
  (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(out x-1 
     (scale-tree x-1 2))

(out x-2 
     (scale-tree x-2 2))
(newline)

(define (scale-tree-2 tree factor)
  (map (lambda (sub-tree)
         (if (list? sub-tree)
           (scale-tree-2 sub-tree factor)
           (* sub-tree factor)))
       tree))

(out x-1 
     (scale-tree-2 x-1 2))

(out x-2 
     (scale-tree-2 x-2 2))

(end-script)
