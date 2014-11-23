(load "./simu_lower_patch.scm")
(load "./gc-code.scm")

(load "./gc-registers.scm")

;; registers that shouldn't be stored by "root" register
(define machine-reserved-gc-registers
  (remove-duplicates
   `(gc-resume-point
     gc-flag
     ,@(extract-register-names gc-code))))

(define default-ops-builder
  (let ((old-builder default-ops-builder))
    (lambda (m)
      `((broken-heart? ,(lambda (sym)
                          (eq? sym machine-gc-broken-heart)))
        (debug-gc-start ,(lambda ()
                           (out "GC triggered")))
        (debug-gc-end ,(lambda ()
                         (format
                          #t
                          "GC done (~A/~A live cells)~%"
                          (machine-pointer-get
                           (machine-reg-get m 'free))
                          machine-memory-size)))
        ,@(old-builder m)))))

(define machine-memory-size 512)

(define (machine-fresh-start! m)
  ;; initialize two pieces of memories
  (machine-reg-set! m 'free (machine-pointer 0))
  ;; we now needs 4 registers to store memories of the same size
  ;; so that we can flip them alternatively.
  (machine-reg-set! m 'the-cars (make-vector machine-memory-size))
  (machine-reg-set! m 'the-cdrs (make-vector machine-memory-size))
  (machine-reg-set! m 'new-cars (make-vector machine-memory-size))
  (machine-reg-set! m 'new-cdrs (make-vector machine-memory-size))
  (machine-reg-set! m 'the-stack '())
  (machine-reset-pc! m)
  (machine-execute! m))

;; user should avoid using any register whose name is prefixed with "gc-"
(define (machine-do-insn-list-preprocess insns)
  (let* ((expanded-insns
          (gen-insn-add-monitors
           (rewrite-instructions* all-rules insns)))
         (prog-entry-label (gensym))
         ;; note that "extract-register-names" removes pc and flag registers
         ;; for pc register, we don't need to store it,
         ;; but we need to keep flag register
         ;; as gc routine might use it.
         ;; therefore a new register is introduced (namely "gc-flag"),
         ;; which keeps the original "flag" register
         (user-registers
          (remove-duplicates
           (set-diff (extract-register-names expanded-insns)
                     machine-reserved-gc-registers)))
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
    `(;; initialization code here
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
      (assign gc-temp (op to-ptr) (const ,machine-memory-size))
      (test (op ptr=?) (reg free) (reg gc-temp))
      (branch (label gc-needed))
      (goto (label gc-all-done))
      gc-needed
      ;; the following instruction does nothing but
      ;; print out a message indicating GC is happening
      (perform (op debug-gc-start))
      ,@save-registers-insns
      ,@gc-code
      ,@restore-registers-insns
      ;; the following instruction does nothing but
      ;; print out the result after GC is done
      (perform (op debug-gc-end))
      gc-all-done
      (assign flag (reg gc-flag))
      (goto (reg gc-resume-point))
      ;; program entry
      ,prog-entry-label
      ,@expanded-insns)))

;; Local variables:
;; proc-entry: "./gc-test-machine.scm"
;; End:

