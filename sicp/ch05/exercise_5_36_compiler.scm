;; compiler patch for ex 5.36

(define primitive-operations
  '(false?
    lookup-variable-value
    set-variable-value!
    define-variable!
    make-compiled-procedure
    compiled-procedure-env
    extend-environment
    list
    ;; cons
    snoc
    compiled-procedure-entry
    primitive-procedure?
    apply-primitive-procedure))

(define (snoc xs v)
  (append xs (list v)))

(define (compile-application exp target linkage)
  ;; takes a list of compiled operand-codes
  ;; and initialize "argl" properly
  (define (construct-arglist operand-codes)
    ;; compiling the argument list is a little bit tricky
    ;; because instead of "wasting an instruction by initializing
    ;; 'argl' to the empty list", the book wastes a few pages
    ;; explaining the compilcation incurred by using this weird order of
    ;; argument evaluation. And I wasted few lines here complaining about it.
    ;; (let ((operand-codes (reverse operand-codes)))
      ;; NOTE: from now on (inside this s-exp)
      ;; the "operand-codes" is reverded.
      (if (null? operand-codes)
          ;; no more operands are required, simply
          ;; assigning "argl" an empty list to continue
          (make-instruction-sequence
           '() '(argl)
           '((assign argl (const ()))))
          (let ((code-to-get-last-arg
                 (append-instruction-sequences
                  (car operand-codes)
                  (make-instruction-sequence
                   '(val) '(argl)
                   '((assign argl (op list) (reg val)))))))
            (if (null? (cdr operand-codes))
                ;; this is the last argument
                code-to-get-last-arg
                ;; this is not the last argument
                (preserving
                 ;; all final linkages of subexpressions are just "next"s
                 ;; no need for keeping "continue" anyway.
                 ;; this can be tested by evaluating some function application
                 ;; whose operands are again function applications.
                 '(env)
                 code-to-get-last-arg
                 ;; merge in rest of the argument evaluations
                 (code-to-get-rest-args
                  (cdr operand-codes)))))));;)
  ;; inlining this procedure because there's no good reason
  ;; to leave it outside. Since "operand-codes" passed to it
  ;; is always reversed, reading the code without context
  ;; doesn't make any sense.
  (define (code-to-get-rest-args operand-codes)
    (let ((code-for-next-arg
           (preserving
            '(argl)
            (car operand-codes)
            (make-instruction-sequence
             '(val argl) '(argl)
             '((assign argl
                       (op snoc) (reg argl)  (reg val)))))))
      (if (null? (cdr operand-codes))
          code-for-next-arg
          (preserving
           '(env)
           code-for-next-arg
           (code-to-get-rest-args (cdr operand-codes))))))
  ;; ====
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
         (map
          (lambda (operand)
            (compile operand 'val 'next))
          (operands exp))))
    (preserving
     '(env continue)
     ;; evaluate operator
     proc-code
     (preserving
      '(proc continue)
      ;; evaluate operands
      (construct-arglist operand-codes)
      ;; dispatch to either a primitive procedure or composite one
      (compile-procedure-call target linkage)))))

;; Local variables:
;; proc-entry: "./exercise_5_36.scm"
;; End:
