;; we are trying to generate instruction lists
;; that maintains register values here.

;; TODO: might be a temporary module, might be merged into
;; other modules when the impl is done
(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./rewrite.scm")
(load "./rewrite-instructions.scm")
(load "./list-stack-rewrites.scm")

;; duplicate value v to form a list of n elements
(define (replicate n v)
  (if (<= n 0)
      '()
      (cons v (replicate (sub1 n) v))))

;; reg-count: the length of the list that
;; we are going to pre-allocate
(define (root-preallocator reg-count)
  `(pre-allocate-root-list
    ,@(tree->instruction-list
       ;; the value doesn't matter, we will fill it right before
       ;; gc takes place
       (replicate reg-count 0))
    (assign root (reg result))))

(for-each out (root-preallocator 3))
