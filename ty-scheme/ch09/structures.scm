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

; default initialization

(defstruct tree2
  height girth age (leaf-shape 'frond) (leaf-color 'green))

; print-field prints field-value pair of a tree2
; we need to use the symbol as a function:
; http://stackoverflow.com/questions/1045069/how-do-i-apply-a-symbol-as-a-function-in-scheme
; but "eval" in mit-scheme need an environment, that is "nearest-repl/environment"
; please refer to 
; http://sicp.ai.mit.edu/Fall-2004/manuals/scheme-7.5.5/doc/scheme_14.html#SEC128
(define print-field2
  (lambda (tree field-sym)
    (display field-sym)
    (display ": ")
    (display (
      (eval 
        (string->symbol (string-append "tree2." (symbol->string field-sym)))
        (nearest-repl/environment)) tree))
    (newline)))

(define print-tree2
  (lambda (tree2)
    (map
      (lambda (f) (print-field2 tree2 f))
    '(height girth age leaf-shape leaf-color))))

(define palm (make-tree2 'height 60))

(print-tree2 palm)

(define plantain
  (make-tree2
    'height 7
    'leaf-shape 'sheet))

(print-tree2 plantain)
