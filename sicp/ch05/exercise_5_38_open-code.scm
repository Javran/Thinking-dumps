;; spread-arguments takes a list of operands
;; and assign each of them into the corresponding
;; target registers. a list of compiled instruction sequences
;; is returned. you need to append them together and do register preserving.
;; * the length of the operand list must be equal to 2
;; * this function assumes the existence of register "arg1" and "arg2"
;;   which the target machine should provide
(define (spread-arguments operand-exps)
  (let ((arg-list-len (length operand-exps)))
    (assert (= arg-list-len 2)
            "'spread-arguments' can only deal with two operands")
    (map (lambda (operand-exp target)
           (compile operand-exp target 'next))
         operand-exps
         ;; it's valid to use "arg1" or "arg2" as target register
         ;; the only thing that the compile has assumed on register usage
         ;; is that with linkage="return",
         ;; the target register must be in "val"
         '(arg1 arg2))))

;; open code compiler generator for binary functions
;; takes a binary operation symbol, and generates a compiler
;; procedure that does open-code compiling for that symbol
;; note that in order to keep the order of
;; operands' evaluation consistent,
;; the operands are evaluated from right to left
;; e.g. to compile arguments for the expression:
;;   (= <exp1> <exp2>)
;; first evaluates "exp2", assigning the value to
;; "arg1" register, and then "exp1" is evaluated
;; and its value is assigned to "arg2"
(define (generate-open-code-compiler-for-binary bin-op)
  (lambda (exp target linkage)
    (let* ((rator (car exp))
           (rands (cdr exp))
           (compiled-rands (spread-arguments rands)))
      ;; since the generator is only for binary functions
      (assert (= (length rands) 2))
      (end-with-linkage
       linkage
       (append-instruction-sequences
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
