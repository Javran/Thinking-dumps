(load "../common/utils.scm")
(load "../common/defstruct.scm")

(defstruct tree height girth age leaf-shape leaf-color)
(defstruct test-str (a '(1 2)) b)

(define coconut
  (make-tree 
    'height 30
    'leaf-shape 'frond
    'age 5))

(out 
  (tree.height coconut)
  (tree.leaf-shape coconut)
  (tree.girth coconut))
; 30
; frond
; <undefined>


(set!tree.height coconut 40)
(set!tree.girth coconut 10)

(out
  (tree.height coconut)
  (tree.girth coconut))
; 40
; 10

(out
  (tree? coconut)
  (tree? 'tree!))
; #t
; #f
