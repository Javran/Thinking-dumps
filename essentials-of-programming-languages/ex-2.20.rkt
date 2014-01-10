#lang eopl

(require "./common.rkt")

; interfaces:
; * number->tree
; * current-element
; * move-to-left
; * move-to-right
; * insert-to-left
; * insert-to-right
; * move-up
; * at-leaf?
; * at-root?

; representation:
; a pair:
; ( <current tree> . [history stack] )
; for each stack element:
;   either:
;   (list 'left <previous root> <previous right tree>)
;   or: 
;   (list 'right <previous root> <previous left tree>)

; the-leaf: () -> Bintree
(define (the-leaf)
  '())

; number->tree: Int -> Bintree
(define (number->tree n)
  (cons (list n '() '())
        '()))

; current-element: Bintree -> Int
(define current-element caar)

; move-to-left: Bintree -> Bintree
(define (move-to-left tree)
  (let ((the-tree (car tree))
        (the-stack (cdr tree)))
    (let ((root (car the-tree))
          (lson (cadr the-tree))
          (rson (caddr the-tree)))
      (cons
        ; the tree
        lson
        ; the stack
        (cons
          (list 'left root rson)
          the-stack)))))

; move-to-right: Bintree -> Bintree
(define (move-to-right tree)
  (let ((the-tree (car tree))
        (the-stack (cdr tree)))
    (let ((root (car the-tree))
          (lson (cadr the-tree))
          (rson (caddr the-tree)))
      (cons
        ; the tree
        rson
        ; the stack
        (cons
          (list 'right root lson)
          the-stack)))))

; insert-to-left: Int x Bintree -> Bintree
(define (insert-to-left x tree)
  (let ((the-tree (car tree))
        (the-stack (cdr tree)))
    (let ((root (car the-tree))
          (lson (cadr the-tree))
          (rson (caddr the-tree)))
      (cons
        ; the tree
        (list root
              (list x
                    lson
                    '())
              rson)
        ; the stack
        the-stack))))

; insert-to-right: Int x Bintree -> Bintree
(define (insert-to-right x tree)
  (let ((the-tree (car tree))
        (the-stack (cdr tree)))
    (let ((root (car the-tree))
          (lson (cadr the-tree))
          (rson (caddr the-tree)))
      (cons
        ; the tree
        (list root
              lson
              (list x
                    '()
                    rson))
        ; the stack
        the-stack))))

; move-up: Bintree -> Bintree
(define (move-up tree)
  (let ((the-tree (car tree))
        (the-stack (cdr tree)))
    (let* ((prev-parts (car the-stack))
           (prev-move (car prev-parts))
           (prev-root (cadr prev-parts))
           (prev-rest (caddr prev-parts)))
      (cons
        ; the tree
        (cond ((eq? prev-move 'left)
                ; restore the root and the right son
                (list prev-root the-tree prev-rest))
              ((eq? prev-move 'right)
                ; restore the root and the left son
                (list prev-root prev-rest the-tree)))
        ; the stack
        (cdr the-stack)))))

; at-leaf?: Bintree -> Bool
(define (at-leaf? tree)
  (null? (car tree)))

; at-root?: Bintree -> Bool
(define (at-root? tree)
  ; the stack is empty?
  (null? (cdr tree)))

; bintree->string: Bintree -> String
(define (bintree->string tree)
  (let ((the-tree (car tree)))
    (if (at-leaf? tree)
      "()"
      (let ((root (car the-tree))
            (lson (cadr the-tree))
            (rson (caddr the-tree)))
        (string-append
          "("
          (number->string root)
          " "
          ; use fake stack as placeholder
          (bintree->string (cons lson '()))
          " "
          (bintree->string (cons rson '()))
          ")")))))

; try to build this tree:
;     6 
;    / \
;   3   4
;  / \   \
; 1   2   5

; first two layers:
(define tree-1
  (insert-to-right
    4
    (insert-to-left
      3
      (number->tree 6))))

; move focus to left,
; insert 1 and 2,
; and move up
(define tree-2
  (move-up
    (insert-to-right
      2
      (insert-to-left
        1
        (move-to-left tree-1)))))

; move focus to right,
; insert 5,
; and move up
(define tree-3
  (move-up
    (insert-to-right
      5
      (move-to-right tree-2))))

(out (bintree->string tree-3))
