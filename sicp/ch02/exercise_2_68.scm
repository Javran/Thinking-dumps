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
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit: CHOOSE-BRANCH" bit))))
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

; ------------------ solution begin

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (not-found symbol)
    (error "symbol not found: " symbol))
  (if (leaf? tree)
    ; if we've reached a leaf
    (if (eq? (symbol-leaf tree) symbol)
      ; the choice has been made, nothing to do
      '()
      (not-found symbol))
    ; else we try to find the symbol
    ;   in either left-branch or right-branch
    (if (memq symbol (symbols-code-tree tree))
      ; symbol must be a member of the symbol list
      (if (memq symbol (symbols (left-branch tree)))
        ; now we're confident that the symbol must be in
        ;   either left-branch or right-branch of the tree
        (cons 0 (encode-symbol symbol (left-branch tree)))
        (cons 1 (encode-symbol symbol (right-branch tree))))
      (not-found symbol))))

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
       ; => the final tree
       (tree-fin (make-code-tree td tree-2))
       ; original data:
       (ori-data '(d c d b b c a d c d))
       ; dcdbbcadcd
       (enc-result (encode ori-data tree-fin))
       (dec-result (decode enc-result tree-fin))
       (correct (equal? ori-data dec-result)))
  (out "Original data:"
       ori-data
       "Encoded data:"
       enc-result
       "Decoded data:"
       dec-result
       "Correct?"
       correct)
  ; uncomment the next line to see the error signal
  ; (out (encode '(a b c d e) tree-fin))
  )

(end-script)
