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

; mark-leaves-with-red-depth :: Bintree -> Bintree
; usage: return a tree of same shape, with the content of every leave
;   changed to the number of nodes whose node is `red` along the path from
;   root to that leaf.
(define (mark-leaves-with-red-depth tree)
  ; same as `mark-leaves-with-red-depth`, but
  ;   with the context that we have reached `red-count` nodes whose
  ;   symbol is `red`
  (define (mark-red-depth-aux red-count tree)
    (if (leaf? tree)
      (leaf red-count)
      (let ((new-red-count
              (if (eq? (contents-of tree) 'red)
                (+ red-count 1)
                red-count)))
        (interior-node
          (contents-of tree)
          (mark-red-depth-aux new-red-count (lson tree))
          (mark-red-depth-aux new-red-count (rson tree))))))
  (mark-red-depth-aux 0 tree))

; bintree->string :: Bintree -> String
; usage: convert Bintree to its string representation
(define (bintree->string tree)
  (if (leaf? tree)
    (number->string (contents-of tree))
    (string-append
      "("
      (symbol->string (contents-of tree))
      " "
      (bintree->string (lson tree))
      " "
      (bintree->string (rson tree))
      ")"))) 

(let ((tree1
        (interior-node 'red
          (interior-node 'bar
            (leaf 26)
            (leaf 12))
          (interior-node 'red
            (leaf 11)
            (interior-node 'quux
              (leaf 117)
              (leaf 14))))))
  (out (bintree->string tree1)
       (bintree->string (mark-leaves-with-red-depth tree1))
       ))
