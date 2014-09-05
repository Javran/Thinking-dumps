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
      (list? l)
      (non-empty? l)
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

(define (test-first-dup-element)
  (do-test
   first-dup-element
   (list
    (mat '() #f)
    (mat '(a b c d) #f)
    (mat '(a b a d) '(a b a d))
    (mat '(a b c d e c) '(c d e c)))))

(if *simu-test*
    (test-first-dup-element)
    'ignored)
