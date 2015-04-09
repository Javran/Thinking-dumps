;; new primitives

;; of form: (compile-and-run (quote <exp>))
(define compile-and-run?
  (list-tagged-with 'compile-and-run))

(define (compile-and-run-exp obj)
  ;; one cadr for destructing "compile-and-run"
  ;; one cadr for destructing "quote"
  (cadr (cadr obj)))
