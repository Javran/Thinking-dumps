(load "../common/utils.scm")
(load "defstruct.scm")

(defstruct tree height girth age leaf-shape leaf-color)

(define coconut
  (make-tree 
    'height 30
    'leaf-shape 'frond
    'age 5))

(out (tree.height coconut))
; 30

(out (tree.leaf-shape coconut))
; frond

(out (tree.girth coconut))
; <undefined>

(set!tree.height coconut 40)
(set!tree.girth coconut 10)

(out (tree.height coconut))
; 40
(out (tree.girth coconut))
; 10
