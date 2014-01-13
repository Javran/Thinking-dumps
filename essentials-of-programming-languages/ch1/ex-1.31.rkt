#lang eopl

(require "../common.rkt")

; leaf :: Int -> Bintree
; usage: create a leaf Bintree
(define (leaf x)
  (list 'leaf x))

; interior-node :: (Sym, Bintree, Bintree) -> Bintree
; usage: create a non-leaf Bintree
(define (interior-node x l r)
  (list 'interior x l r))

; leaf? :: Bintree -> Bool
; usage: test if the Bintree is a leaf
(define (leaf? t)
  (eq? 'leaf (car t)))

; lson :: Bintree -> Bintree
; usage: fetch the left son
(define (lson x)
  (list-ref x 2))

; rson :: Bintree -> Bintree
; usage: fetch the right son
(define (rson x)
  (list-ref x 3))

; contents-of :: Bintree -> Either Sym Int
; usage: fetch the content from Bintree `x`
(define (contents-of x)
  (list-ref x 1))

;      b
;     / \
;    /   \
;   a     c
;  / \   / \
; 1   2 3   d
;          / \
;         4   5
(let ((tree (interior-node 'b 
              (interior-node 'a
                (leaf 1)
                (leaf 2))
              (interior-node 'c
                (leaf 3)
                (interior-node 'd
                  (leaf 4)
                  (leaf 5))))))
  (out (leaf? tree)
       ; #f
       (leaf? (lson (lson tree)))
       ; #t
       (contents-of tree)
       ; b
       (contents-of (lson (rson tree)))
       ; 3
       ))
