#lang eopl

(require "./common.rkt")

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

; double-tree :: Bintree -> Bintree
; usage: double every leaf in `tree`
(define (double-tree tree)
  (if (leaf? tree)
    (leaf (* 2 (contents-of tree)))
    (interior-node
      (contents-of tree)
      (double-tree (lson tree))
      (double-tree (rson tree)))))

;     a
;    / \
;   b   3
;  / \
; 1   2
(let ((tree (interior-node 'a
              (interior-node 'b
                (leaf 1)
                (leaf 2))
              (leaf 3))))
  (out tree)
  (out (double-tree tree)))
