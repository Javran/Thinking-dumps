;; this module transforms the program instruction list
;; into one that has builtin GC functionality

(load "./gc-code.scm")

;; generate an instruction list transformer
;; that transforms original program
;; into a lower level one equiped with automatic garbage collection
;; arguments: "broken-heart-symbol" should be an unique symbol
;; that used internally by garbage collecting algorithm
;; "mem-size" is the least memory size among 4 memories
;; (note that usually these 4 memories have the same size)
(define (gc-transform-program-with
         broken-heart-symbol
         mem-size)
  (lambda (insns)
    ;; the following help function
    ;; generate instruction lists to take care of
    ;; all registers that are not related to the garbage collector

    ;; registers that shouldn't be stored by "root" register
    ;; two registers are special: pc and flag
    ;; for pc register, we don't need to store it,
    ;; but we need to keep flag register
    ;; as gc routine might use it.
    ;; therefore a new register is introduced (namely "gc-flag"),
    ;; which keeps the original "flag" register
    (define reserved-gc-registers
      (remove-duplicates
       `(pc
         flag
         gc-resume-point
         gc-flag
         ,@(extract-register-names (gen-gc-code 'dummy-symbol)))))

    ;; generate an instruction list that pre-allocates
    ;; a list for storing all registers that are not
    ;; related to the garbage collecting algorithm.
    ;; this generated list should be executed when the machine starts
    ;; reg-count: the length of the list that
    ;; we are going to pre-allocate
    ;; after the execution of this part is done,
    ;; "root" register should be pointing to the preallocated list
    (define (gen-insn-root-preallocator reg-count)
      `(pre-allocate-root-list
        ,@(tree->instruction-list
           ;; the value doesn't matter, we will fill it right before
           ;; gc takes place
           (replicate reg-count 0))
        (assign root (reg result))))

    ;; right before entering the garbage collecting phase,
    ;; we need instructions that moves values from registers to
    ;; the preallocated list indicated by "root" register
    ;; note that argument "regs" should be exactly the same as one
    ;; passed to "gen-insn-restore-registers-from-root"
    (define (gen-insn-save-registers-to-root regs)
      (define (save-regs-intern regs)
        (if (null? regs)
            '()
            `( (perform (op set-car!) (reg gc-temp) (reg ,(car regs)))
               (assign gc-temp (op cdr) (reg gc-temp))
               ,@(save-regs-intern (cdr regs))
               )))
      `(gc-save-registers-to-root
        (assign gc-temp (reg root))
        ,@(save-regs-intern regs)))

    ;; right after the garbage collecting phase is done,
    ;; we move values from list indicated by "root" to
    ;; registers.
    ;; note that argument "regs" should be exactly the same as one
    ;; passed to "gen-insn-save-registers-to-root"
    (define (gen-insn-restore-registers-from-root regs)
      (define (restore-regs-intern regs)
        (if (null? regs)
            '()
            `( (assign ,(car regs) (op car) (reg gc-temp))
               (assign gc-temp (op cdr) (reg gc-temp))
               ,@(restore-regs-intern (cdr regs))
               )))
      `(gc-restore-registers-from-root
        (assign gc-temp (reg root))
        ,@(restore-regs-intern regs)))

    ;; generate instructions that checks if we still have
    ;; free spaces available after every time free pointer gets increased.
    ;; if no free space is left, garbage collector will be called.
    ;; The generated code is also responsible for resuming to the point
    ;; before garbage collector get started so the program can proceed
    ;; with some newly freed space.
    (define (gen-insn-check-free-space insns)
      (define (expand-insn insn)
        ;; we are only interested in
        ;; the increment of "free"
        (if (equal? insn '(assign free (op ptr-inc) (reg free)))
            ;; labels for resuming points are generated
            ;; whose uniqueness can be guaranteed.
            (let ((jump-back-label (gensym)))
              `((assign free (op ptr-inc) (reg free))
                ;; go to the subroutine to check the free space
                ;; and subroutine use the location stored in "gc-resume-point"
                ;; to resume.
                (assign gc-resume-point (label ,jump-back-label))
                (goto (label gc-maybe-start))
                ,jump-back-label
                ))
            (list insn)))
      (concat-map expand-insn insns))

    (let* ((expanded-insns
            (gen-insn-check-free-space
             (rewrite-instructions* all-rules insns)))
           (prog-entry-label (gensym))
           (user-registers
            (remove-duplicates
             (set-diff (extract-register-names expanded-insns)
                       reserved-gc-registers)))
           ;; we assume preallocating space for register traversal
           ;; will never used up the space
           ;; and this assumption is almost safe as long as we have
           ;; enough space for all user registers
           (root-preallocate-insns
            (rewrite-instructions*
             all-rules
             (gen-insn-root-preallocator (length user-registers))))
           ;; instructions to save registers to root
           (save-registers-insns
            (rewrite-instructions*
             all-rules
             (gen-insn-save-registers-to-root user-registers)))
           ;; instructions to restore registers from root
           (restore-registers-insns
            (rewrite-instructions*
             all-rules
             (gen-insn-restore-registers-from-root user-registers)))
           )
      `(
        ;; initialization code here
        ;; preallocate spaces for storing registers
        ;; during the gc phase.
        ;; we assume the preallocation will never use up
        ;; memory space, thus no code inserted in this part
        ;; to check if `free` register is out of range
        ,@root-preallocate-insns
        (goto (label ,prog-entry-label))
        ;; all subroutines
        gc-maybe-start
        (assign gc-flag (reg flag))
        ;; everytime this subroutine is triggered,
        ;; "free" register is suppose to point to the next
        ;; free space, and if free = memory size, that means
        ;; we have used up all the free spaces and needs to
        ;; perform garbage collection immediately
        (assign gc-temp (op to-ptr) (const ,mem-size))
        (test (op ptr=?) (reg free) (reg gc-temp))
        (branch (label gc-needed))
        (goto (label gc-all-done))
        gc-needed
        ;; the following instruction does nothing but
        ;; print out a message indicating GC is happening
        (perform (op debug-gc-start))
        ,@save-registers-insns
        ,@(gen-gc-code broken-heart-symbol)
        ,@restore-registers-insns
        ;; the following instruction does nothing but
        ;; print out the result after GC is done
        (perform (op debug-gc-end))
        gc-all-done
        (assign flag (reg gc-flag))
        (goto (reg gc-resume-point))
        ;; program entry
        ,prog-entry-label
        ,@expanded-insns))))
