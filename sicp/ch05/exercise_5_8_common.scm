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

(define (scan-duplicate-labels insns label?)
  (let* ((labels (filter label? insns))
         (dup-lbl (first-dup-element labels)))
    (if dup-lbl
        ;; here we can even report all the labels with
        ;; the same name, but I don't find it not very useful.
        (error "multiple labels with the same name:"
               (car dup-lbl))
        'ok)))
