#lang eopl

(require "../common.rkt")

; interfaces:
; * number->bintree
; * current-element
; * move-to-left-son
; * move-to-right-son
; * at-leaf?
; * insert-to-left
; * insert-to-right

; the-leaf: () -> Bintree
(define (the-leaf)
  '())

; number->bintree: Int -> Bintree
(define (number->bintree n)
  (list n (the-leaf) (the-leaf)))

; current-element: Bintree -> Int
(define current-element car)

; move-to-left-son: Bintree -> Bintree
(define move-to-left-son cadr)

; move-to-right-son: Bintree -> Bintree
(define move-to-right-son caddr)

; at-leaf?: Bintree -> Bool
(define at-leaf? null?)

; insert-to-left: Int x Bintree -> Bintree
; usage: this function is not well defined.
;   so just define something that suits the expected output.
;   must be called on non-leaf nodes
;   replace left son with new tree
;   the original left son is attached to the new tree's left son
(define (insert-to-left n tree)
  (let ((root (car tree))
        (lson (cadr tree))
        (rson (caddr tree)))
    (list root
          (list n
                lson 
                '())
          rson)))

; insert-to-right: Int x Bintree -> Bintree
; same problme as `insert-to-left`
(define (insert-to-right n tree)
  (let ((root (car tree))
        (lson (cadr tree))
        (rson (caddr tree)))
    (list root
          lson
          (list n
                '()
                rson))))

(define t1
  (insert-to-right
    14
    (insert-to-left
      12
      (number->bintree 13))))

(out
  t1
  (move-to-left-son t1)
  (current-element (move-to-left-son t1))
  (at-leaf? (move-to-right-son (move-to-left-son t1)))
  (insert-to-left 15 t1))
