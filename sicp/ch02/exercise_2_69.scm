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

; my solution can be broken down into 2 parts:
; * the implementation of ordered set
;   exactly what we've done before
; * the implementation of successive merge process
;   given that the set is sorted (ordered) by its frequency,
;   we simply take and remove the first two elements from list,
;   and insert the resulting tree into the set again until there's only one tree remaining

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge branch-set)
  (let ((len (length branch-set)))
    (cond ((= len 0)
            (error "nothing to merge"))
          ((= len 1)
            (car branch-set))
          (else
            ; the set has at least 2 elements
            (let ((fst (car branch-set))
                  (snd (cadr branch-set))
                  (rest (cddr branch-set)))
              (successive-merge
                (cons (make-code-tree fst snd)
                      rest)))))))

(let ((tt 
       ; here we can passed a shuffled version of initial pair list
       (list (list 'A 4)
             (list 'C 1)
             (list 'B 2)
             (list 'D 1))))
  (out (generate-huffman-tree tt)
       ))

(end-script)
