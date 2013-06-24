(load "../common/utils.scm")

; a set is a data structure that supports:
; union-set
; intersection-set
; element-of-set?
; adjoin-set

; tree definition and corresponding accessors
(define (make-tree entry left right)
  (list entry left right))

(define entry car)
(define left-branch cadr)
(define right-branch caddr)

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
          (element-of-set? x (left-branch set)))
        ((> x (entry set))
          (element-of-set? x (right-branch set)))))

(let ((set (make-tree 2
                    (make-tree 1 nil nil)
                    (make-tree 3 nil nil))))
  (out (map
         (lambda (x)
           (element-of-set? x set))
         (list-in-range 1 4))))
  ; (#t #t #t #f)
(newline)

(define (adjoin-set x set)
  (cond ((null? set)
          (make-tree x nil nil))
        ((= x (entry set))
          set)
        ((< x (entry set))
          (make-tree (entry set)
                     (adjoin-set x (left-branch set))
                     (right-branch set)))
        ((> x (entry set))
          (make-tree (entry set)
                     (left-branch set)
                     (adjoin-set x (right-branch set))))))

; make a full bin tree in range 3-9
(let ((s (fold-right adjoin-set nil (reverse '(6 4 8 3 7 5 9)))))
  (out s)
  (out (map
         (lambda (x) (element-of-set? x s))
         (list-in-range 1 10))))
  ; #f for first two and the last element

(out "=======")
(newline)

; for union-set and intersection-set,
;   we first flatten two tree, then do the corresponding operation,
;   after that we convert the result to a balanced tree
(define (tree->list tree)
  ; exercise 2.63: tree to list method-1
  (if (null? tree)
    nil
    (append (tree->list (left-branch tree))
            (cons (entry tree)
                  (tree->list
                    (right-branch tree))))))

(define (list->tree elements)
  ; exercise 2.64: list to tree
  (define (partial-tree elts n)
    (if (= n 0)
      (cons nil elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
                (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                    (partial-tree
                      (cdr non-left-elts)
                      right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                      (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))
  (car (partial-tree elements (length elements))))

(let ((t (make-tree 1
                    nil
                    (make-tree 2
                               nil
                               (make-tree 3
                                          nil
                                          (make-tree 4
                                                     nil
                                                     (make-tree 5 nil nil)))))))
  (out (tree->list t))
  ; (1 2 3 4 5)
  ; flatten-convert-back actually "optimizes" the structure of the tree
  (out (list->tree (tree->list t)))
  ; (3 (1 () (2 () ())) (4 () (5 () ())))
  )

; 2.3.3 intersection-set for ordered list
(define (intersection-set-ol set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
              (cons x1 (intersection-set-ol (cdr set1)
                                         (cdr set2))))
            ((< x1 x2)
              (intersection-set-ol (cdr set1) set2))

            ((< x2 x1)
              (intersection-set-ol set1 (cdr set2)))))))

(define (intersection-set set1 set2)
  (let ((set-l1 (tree->list set1))
        (set-l2 (tree->list set2)))
    (list->tree (intersection-set-ol set-l1 set-l2))))

; exercise 2.62: union-set for ordered list
(define (union-set-ol set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set-ol (cdr set1) (cdr set2))))
                      ((< x1 x2)
                       (cons x1 (union-set-ol (cdr set1) set2)))
                      ((> x1 x2)
                       (cons x2 (union-set-ol set1 (cdr set2)))))))))
(define (union-set set1 set2)
  (let ((set-l1 (tree->list set1))
        (set-l2 (tree->list set2)))
    (list->tree (union-set-ol set-l1 set-l2))))


(let ((t1 (list->tree (gen-list 1 7 2)))
      (t2 (list->tree (list-in-range 4 7))))
  (out (intersection-set t1 t2))
  ; (1 3 5 7) /\ (4 5 6 7) => (5 7)
  (out (union-set t1 t2))
  ; (1 3 5 7) \/ (4 5 6 7) => (1 3 4 5 6 7)
  )

(end-script)
