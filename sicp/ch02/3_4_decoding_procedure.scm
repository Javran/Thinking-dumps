(load "../common/utils.scm")

; excerpted from previous code, compact version for copying
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define symbol-leaf cadr)
(define weight-leaf caddr)
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left)
           (weight right))))
(define left-branch car)
(define right-branch cadr)
(define symbols-code-tree caddr)
(define weight-code-tree cadddr)
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (symbols-code-tree tree)))
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (weight-code-tree tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      ; no more bits
      '()
      (let ((next-branch
              ; choose a branch according to the current bit
              (choose-branch
                (car bits)
                current-branch)))
        (if (leaf? next-branch)
          ; leaf reached, return symbol as part of the result,
          ;   then go on by passing the whole tree again (back to the root)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          ; elsewise just move to the next branch
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

; test:
; a:1 b:2 c:3 d:4
(let* ((ta (make-leaf 'a 1))
       (tb (make-leaf 'b 2))
       (tc (make-leaf 'c 3))
       (td (make-leaf 'd 4))
       ; merge a,b: (a b):3 c:3 d:4
       (tree-1 (make-code-tree ta tb))
       ; merge (a b),c: ((a b) c):6 d:4
       (tree-2 (make-code-tree tree-1 tc))
       ; merge all: (d ((a b) c))
       (tree-3 (make-code-tree td tree-2))
       ; d: 0
       ; c: 11
       ; a: 100
       ; b: 101
       ; dcdbbcadcd
       ; => 0 11 0 101 101 11 100 0 11 0
       ; => 0110 1011 0111 1000 110
       (result (decode '(0 1 1 0
                         1 0 1 1
                         0 1 1 1
                         1 0 0 0
                         1 1 0)
                       tree-3))
       (expected '(d c d b b c a d c d)))
  (out result expected)
  (out (equal? result expected)))

(end-script)
