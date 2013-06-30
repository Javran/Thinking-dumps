(load "../common/utils.scm")

; excerpted from ex 2.69
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

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
          (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)   ; symbol
                             (cadr pair)) ; frequency
                  (make-leaf-set (cdr pairs))))))
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge branch-set)
  (let ((len (length branch-set)))
    (cond ((= len 0)
            (error "nothing to merge"))
          ((= len 1)
            (car branch-set))
          (else
            (let ((fst (car branch-set))
                  (snd (cadr branch-set))
                  (rest (cddr branch-set)))
              (successive-merge
                (cons (make-code-tree fst snd)
                      rest)))))))

; excerpted from ex 2.68
(define (decode bits tree)
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit: CHOOSE-BRANCH" bit))))
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch
                (car bits)
                current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (define (not-found symbol)
    (error "symbol not found: " symbol))
  (if (leaf? tree)
    (if (eq? (symbol-leaf tree) symbol)
      '()
      (not-found symbol))
    (if (memq symbol (symbols-code-tree tree))
      (if (memq symbol (symbols (left-branch tree)))
        (cons 0 (encode-symbol symbol (left-branch tree)))
        (cons 1 (encode-symbol symbol (right-branch tree))))
      (not-found symbol))))

; my solution:
(define huff-tree 
  (generate-huffman-tree
    (list (list 'a     2)
          (list 'get   2)
          (list 'sha   3)
          (list 'wah   1)
          (list 'boom  1)
          (list 'job   2)
          (list 'na    16)
          (list 'yip   9))))
(out
  "The Huffman tree is:"
  huff-tree)

(define (test-message msg tree)
  (let* ((enc-result (encode msg tree))
         (dec-result (decode enc-result tree)))
    (out "Original message:"
         msg
         "Length:"
         (length msg)

         "Encoded message:"
         enc-result
         "Length:"
         (length enc-result)

         "Decoded message:"
         dec-result
         "Length:"
         (length dec-result)

         "Correct?"
         (equal? msg dec-result)
         )))

(test-message
  '(
    get a job
    sha na na na na na na na na
    get a job
    sha na na na na na na na na
    wah yip yip yip yip yip yip yip yip yip
    sha boom)
  huff-tree)

(end-script)
