;; modify the specify element in the vector
;; by calling a procedure
(define (vector-modify! vec k proc)
  (vector-set!
   vec
   k
   (proc (vector-ref vec k))))

;; remove duplicate elements
(define (remove-duplicates xs)
  (if (null? xs)
      '()
      (cons (car xs)
            (delete
             (car xs)
             (remove-duplicates (cdr xs))))))

;; xs - ys
(define (set-diff xs ys)
  (fold-right delete xs ys))

;; check if two lists are of the same length
;; (should be more efficient than getting their lengths
;; and then comparing)
(define (same-length? xs ys)
  (cond ((null? xs) (null? ys))
        ;; when we say "pair?", we actually checks
        ;; if "car" and "cdr" can be used on a given object.
        ((and (pair? xs) (pair? ys))
         (same-length? (cdr xs) (cdr ys)))
        (else #f)))

;; ((list-tagged-with <tag>) '(<tag> ...)) => #t
(define (list-tagged-with tag)
  (lambda (l)
    (and
     (pair? l)
     (eq? (car l) tag))))

(define (tagged-list? exp tag)
  ((list-tagged-with tag) exp))

;; find the first duplicated element
;; if no duplicate element is found, return #t
;; otherwise a list will be returned
;; whose first element is the duplicate one
;; e.g. (a b c d) => #f
;;      (a b a d) => (a b a d)
;;      (a b c d e c) => (c d e c)
(define (first-dup-element xs)
  (if (null? xs)
      #f
      (if (member (car xs) (cdr xs))
          xs
          (first-dup-element (cdr xs)))))

;; check a list of instructions including labels
;; * force every label to be unique
;; * warn if the is any undefined by used labels
;; #f will be returned if there is any warning
(define (check-labels insns)
  (let ((labels (filter symbol? insns)))
    ;; if all the labels are unique
    ;; then turning it into a set shouldn't
    ;; make a different on the size of it
    (assert (= (length labels)
               (length (remove-duplicates labels)))
            "labels are supposed to be unique")
    ;; extract a list of labels from a single instruction
    (define (extract-used-labels insn)
      (cond ((and
              ;; it's possible to use "car"
              (pair? insn)
              ;; the instruction is (branch ...) or (goto ...)
              (or (eq? (car insn) 'branch)
                  (eq? (car insn) 'goto))
              ;; more precisely, should be of the following form:
              ;; (<branch|goto> (label <label>) ...)
              ;; "cadr" on the instruction finds "(label <label>)"
              ;; and another "cadr" on this result will extract
              ;; that very label
              (eq? (car (cadr insn)) 'label))
             (list (cadr (cadr insn))))
            (else '())))
    (let ((used-labels
           (remove-duplicates
            (concat-map extract-used-labels insns))))
      ;; are all used labels defined in this list itself?
      (let ((results
             (map
              (lambda (l)
                (if (memq l labels)
                    #t
                    (begin
                      (warn (format #f "label ~A not defined" l))
                      #f)))
              used-labels)))
        (fold-left (lambda (a b)
                     (and a b))
                   #t results)))))

;; extract required operations from a list of instructions
;; operations are represented as (<operation-name> <arity>)
;; NOTE: this procedure assumes the operations can only appear
;; in one of the following forms:
;; - (assign <operation> ...)
;; - (test (op <operation>) ...)
;; - (perform (op <operation>) ...)
;; if you have modified the syntax,
;; this function might not work properly.
(define (extract-operations insns)
  (define (insn->operation insn)
    (if (pair? insn)
        (let ((head (car insn)))
          (cond ((and (eq? head 'assign)
                      (eq? (car (caddr insn)) 'op))
                 ;; (assign _ (op _) ..)
                 (list (list (cadr (caddr insn))
                             (- (length insn) 3))))
                ((or (eq? head 'test)
                     (eq? head 'perform))
                 ;; (test (op _) ..)
                 ;; (perform (op _) ..)
                 (list (list (cadr (cadr insn))
                             (- (length insn) 2))))
                (else
                 '())))
        '()))
  (remove-duplicates
   (concat-map insn->operation
               insns)))

(define (test-first-dup-element)
  (do-test
   first-dup-element
   (list
    (mat '() #f)
    (mat '(a b c d) #f)
    (mat '(a b a d) '(a b a d))
    (mat '(a b c d e c) '(c d e c)))))
