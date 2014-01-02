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

; number-leaves1 :: Bintree -> Bintree
; usage: replace all leaves with increasing numbers, starting from 0
(define (number-leaves1 tree)
  ; continuation approach
  (define (number-leaves-cont tree start-num cont)
    (if (leaf? tree)
      ; call continuation with created tree and the next `start-num`
      (cont (leaf start-num) (+ start-num 1))
      ; build left tree
      (number-leaves-cont
        (lson tree)
        start-num
        (lambda (lson-done cur-num)
          ; build right tree
          (number-leaves-cont
            (rson tree)
            cur-num
            (lambda (rson-done cur-num)
              ; call contiuation with built tree
              (cont
                (interior-node
                  (contents-of tree)
                  lson-done
                  rson-done)
                cur-num)))))))
  (number-leaves-cont
    tree
    0 
    ; fetch result
    (lambda (c num) c)))

; number-leaves2 :: Bintree -> Bintree
; usage: replace all leaves with increasing numbers, starting from 0
(define (number-leaves2 tree)
  ; return a pair: (tree, next-num)
  (define (number-leaves-aux tree start-num)
    (if (leaf? tree)
      (cons (leaf start-num)
            (+ 1 start-num))
      (let* ((lresult (number-leaves-aux (lson tree) start-num))
             (rresult (number-leaves-aux (rson tree) (cdr lresult))))
        (cons (interior-node
                (contents-of tree)
                (car lresult)
                (car rresult))
              (cdr rresult)))))
  (car (number-leaves-aux tree 0)))

(let ((tree1
        (interior-node
          'foo
          (interior-node
            'bar
            (leaf 26)
            (leaf 12))
          (interior-node
            'baz
            (leaf 11)
            (interior-node
              'quux
              (leaf 117)
              (leaf 14)))))
      (tree2
        (interior-node 
          'a
          (leaf 20)
          (interior-node
            'b
            (interior-node
              'c
              (leaf 50)
              (leaf 60))
            (leaf 40)))))
  (out (bintree->string (number-leaves1 tree1))
       (bintree->string (number-leaves2 tree1))
       (bintree->string (number-leaves1 tree2))
       (bintree->string (number-leaves2 tree2))
       ))
