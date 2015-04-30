;;; primitives for the compiler

(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; import primitives from our explicit control evaluator
;; NOTE: the primitives are more than necessary,
;; for example "prompt-for-input" is defined
;; but not actually used in our compiler.
(load "ec-prim.scm")

;; include syntax extensions from ex 5.23
(load "exercise_5_23_common.scm")

;; TODO: developing and/or transformation inplace...


;; based on exercise 4.4,
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
  (cond ((null? exp)
         #f)
        ((null? (cdr exp))
         (car exp))
        (else
         (let ((result-sym (gensym)))
           (list
            (list 'lambda (list result-sym)
                  (list 'if result-sym
                        result-sym
                        (list 'or (or->if (cdr exp)))))
            (car exp))))))

(pp (and->if '(and 1 2 3 4 5)))
