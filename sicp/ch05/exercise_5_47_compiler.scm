(set! all-regs
      (set-insert 'compapp all-regs))
;; TODO: init compapp

(define (ec-get-required-operations)
  (set-union
   ;; fool-proof, prevent duplicated elements
   (remove-duplicates
    ;; primitive-operations copied from the compiler
    ;; as it might get mutated when loading patches.
    '(false?
      lookup-variable-value
      set-variable-value!
      define-variable!
      make-compiled-procedure
      compiled-procedure-env
      extend-environment
      list
      cons
      snoc ;; added for exercise_5_36_compiler.scm
      compiled-procedure-entry
      primitive-procedure?
      apply-primitive-procedure
      ;; operations added in ex 5.47
      compound-procedure?
      compiled-procedure?
      error
      ))
   (map car (extract-operations evaluator-insns))))

(define (compile-procedure-call target linkage)
  ;; function application for compiled procedures
  (define (compile-proc-appl target linkage)
    ;; note that the linkage will never be "next"
    ;; case coverage:
    ;;         | target = val | target != val
    ;; return  | covered      | error
    ;; <other> | covered      | covered
    (cond ((and (eq? target 'val)
                (not (eq? linkage 'return)))
           ;; - no need to move result value
           ;; - do an immediate jump afterwards
           (make-instruction-sequence
            '(proc) all-regs
            `((assign continue (label ,linkage))
              (assign val (op compiled-procedure-entry)
                      (reg proc))
              (goto (reg val)))))
          ((and (not (eq? target 'val))
                (not (eq? linkage 'return)))
           ;; - need to move result value "val" to the target
           ;; - do an immediate jump afterwards
           (let ((proc-return (make-label 'proc-return)))
             (make-instruction-sequence
              '(proc) all-regs
              `((assign continue (label ,proc-return))
                (assign val (op compiled-procedure-entry)
                        (reg proc))
                (goto (reg val))
                ,proc-return
                ;; need this extra step to transfer value
                (assign ,target (reg val))
                (goto (label ,linkage))))))
          ((and (eq? target 'val) (eq? linkage 'return))
           ;; - no need to move result value
           ;; - let the compiled procedure do the return job
           (make-instruction-sequence
            '(proc continue) all-regs
            `((assign val (op compiled-procedure-entry)
                      (reg proc))
              (goto (reg val)))))
          ((and (not (eq? target 'val))
                (eq? linkage 'return))
           ;; - convention is violated here: whenever we return,
           ;;   the resulting value should be available in "val"
           ;; - the only place where a "return" linkage used
           ;;   is in the body of "compile-lambda-body",
           ;;   and that code sets its target to "val".
           ;; so no code (at least for those found in book)
           ;; will hit this branch
           (error "return linkage, target not val: COMPILE"
                  target))))
  (define (compile-compound-proc-appl target linkage)
    ;; note that compile-compound-proc-appl can never be called with
    ;; linkage = next
    (assert (not (eq? linkage 'next))
            "linkage can never be 'next in this function")
    (cond ((and (eq? target 'val)
                (not (eq? linkage 'return)))
           ;; case: target == val && linkage /= return
           (error 'todo)
           )
          ((and (not (eq? target 'val))
                (not (eq? linkage 'return)))
           ;; case: target /= val && linkage /= return
           (error 'todo)
           )
          ((and (eq? target 'val) (eq? linkage 'return))
           ;; case: target == val && linkage == return
           (error 'todo)
           )
          ((and (not (eq? target 'val))
                (eq? linkage 'return))
           ;; case: target /= val && linkage == return
           ;; when the linkage is "return", we should have
           ;; the resulting value in "val", if we ever get
           ;; into this case, the convention is violated
           (error "return linkage, target not val: COMPILE"
                  target))))
  ;; ====
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (compound-branch (make-label 'compound-branch))
        (after-call (make-label 'after-call)))
    ;; INVARIANT: compile-proc-appl and compile-compound-proc-appl
    ;; can never be called with linkage = next
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage))
          (compound-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          ;; goto primitive branch
          (branch (label ,primitive-branch))
          (test (op compiled-procedure?) (reg proc))
          (branch (label ,compiled-branch))
          (test (op compound-procedure?) (reg proc))
          (branch (label ,compound-branch))
          ;; if all of the dispatches have failed ... we panic!
          (perform (op error) (const "unknown proc object"))
          ))
       (parallel-instruction-sequences
        (parallel-instruction-sequences
         ;; ==> deal with compiled procedures
         (append-instruction-sequences
          ;; otherwise the procedure must be a compiled one
          compiled-branch
          ;; note that it's not possible for compiled-linkage to
          ;; take the value "next"
          (compile-proc-appl target compiled-linkage))
         ;; ==> deal with compound procedures
         (append-instruction-sequences
          compound-branch
          (compile-compound-proc-appl target compound-linkage)))
        ;; ==> deal with primitive procedures
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage
          linkage
          (make-instruction-sequence
           '(proc argl) (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))

;; Local variables:
;; proc-entry: "exercise_5_47.scm"
;; End:
