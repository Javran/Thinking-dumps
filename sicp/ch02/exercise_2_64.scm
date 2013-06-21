(load "../common/utils.scm")

(define (make-tree entry left right)
  (list entry left right))

(define entry car)
(define left-branch cadr)
(define right-branch caddr)

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

; partial-tree is a helper procedure that
;   * takes a list of at least n elements
;   * takes an integer n
;   * returns a pair:
;       * car - constructed tree
;       * cdr - element not included in the tree

(define (list->tree elements)
  ; so the procedure should make sure `cdr` of the result contains nothing
  ;   in order to keep the correctness
  (car (partial-tree elements (length elements))))

(define (partial-tree elements n)
  (if (= n 0)
    ; limitation reached
    (cons '() elements)
    (let ((left-size (quotient (- n 1) 2)))
      ; element count for left tree
      (let ((left-result
              (partial-tree elements left-size)))
        (let ((left-tree (car left-result))
              (non-left-elements (cdr left-result))
              ; left-size + 1 + right-size = n
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elements))
                (right-result
                  (partial-tree (cdr non-left-elements)
                                right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elements (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elements))))))))

(out (list->tree '(1 2 3 4 5)))
(out (list->tree (list-in-range 1 10)))

(end-script)
