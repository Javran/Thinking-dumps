;;; primitives for the compiler

;; import primitives from our explicit control evaluator
;; NOTE: the primitives are more than necessary,
;; for example "prompt-for-input" is defined
;; but not actually used in our compiler.
(load "ec-prim.scm")

;; include syntax extensions from ex 5.23
(load "exercise_5_23_common.scm")

(define and? (list-tagged-with 'and))
(define or? (list-tagged-with 'or))

;; based on exercise 4.4
(define (and->if exp)
  (assert (eq? 'and (car exp)))
  (define (exprs->if exps)
    (cond
     ;; and with an empty list of expressions
     ;; should result in #t
     ((null? exps) #t)
     ;; or if it contains exactly one element,
     ;; that element should be used
     ((null? (cdr exps))
      (car exps))
     (else
      ;; a let-expression will do,
      ;; but I think we should better not rely on it.
      `(if ,(car exps)
           ,(exprs->if (cdr exps))
           #f))))
  (exprs->if (cdr exp)))

(define (or->if exp)
  (assert (eq? 'or (car exp)))
  (define (exprs->if exps)
    (cond
     ((null? exps)
      #f)
     ((null? (cdr exps))
      (car exps))
     (else
      ;; here we need to store the value
      ;; or otherwise the same expression
      ;; will be evaluated twice
      ;; (or (begin (display 'x) 1)) for example
      (let ((result-sym (gensym)))
        `((lambda (,result-sym)
            (if ,result-sym
                ,result-sym
                ,(exprs->if (cdr exps))))
          ,(car exps))))))
  (exprs->if (cdr exp)))
