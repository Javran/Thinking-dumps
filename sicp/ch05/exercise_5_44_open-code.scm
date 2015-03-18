;; based on exercise 5.38_open-code.scm

;; ctenv is added for argument compiling
(define (spread-arguments operand-exps ctenv)
  (let ((arg-list-len (length operand-exps)))
    (assert (= arg-list-len 2)
            "'spread-arguments' can only deal with two operands")
    (map (lambda (operand-exp target)
           (compile operand-exp target 'next ctenv))
         operand-exps
         '(arg1 arg2))))

(define (generate-open-code-compiler-for-binary bin-op)
  ;; ctenv added
  (lambda (exp target linkage ctenv)
    (let* ((rator (car exp))
           (rands (cdr exp))
           (compiled-rands (spread-arguments rands ctenv)))
      ;; since the generator is only for binary functions
      (assert (= (length rands) 2))
      (end-with-linkage
       linkage
       (preserving
        '(env continue)
        ;; the second arg first
        (cadr compiled-rands)
        (preserving
         '(arg2)
         (car compiled-rands)
         (make-instruction-sequence
          '(arg1 arg2)
          (list target)
          `( (assign ,target (op ,bin-op) (reg arg1) (reg arg2)) ))))))))

(define (generate-open-code-predicate-for-binary bin-op)
  (lambda (exp)
    (and ((list-tagged-with bin-op) exp)
         (= (length exp) 3))))

;; it might be convenient to use the message dispatching framework
;; but as we only have 4 cases to take care, I don't think
;; it matters that much
(define compile-open-code-bin-op-=
  (generate-open-code-compiler-for-binary '=))
(define compile-open-code-bin-op-*
  (generate-open-code-compiler-for-binary '*))
(define compile-open-code-bin-op--
  (generate-open-code-compiler-for-binary '-))
(define compile-open-code-bin-op-+
  (generate-open-code-compiler-for-binary '+))

(define open-code-bin-op-=?
  (generate-open-code-predicate-for-binary '=))
(define open-code-bin-op-*?
  (generate-open-code-predicate-for-binary '*))
(define open-code-bin-op--?
  (generate-open-code-predicate-for-binary '-))
(define open-code-bin-op-+?
  (generate-open-code-predicate-for-binary '+))
