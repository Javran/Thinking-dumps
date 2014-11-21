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
  (machine-reg-set! m 'the-cars (make-vector machine-memory-size))
  (machine-reg-set! m 'the-cdrs (make-vector machine-memory-size))
  (machine-reg-set! m 'new-cars (make-vector machine-memory-size))
  (machine-reg-set! m 'new-cdrs (make-vector machine-memory-size))
  (machine-reg-set! m 'the-stack '())
  (machine-reset-pc! m)
  (machine-execute! m))

;; TODO: think about how to deal with "continue" register,
;; which stores non-regular values?

;; TODO: forbid access to gc-related registers in user instruction list
(define (machine-do-insn-list-preprocess insns)
  (let* ((expanded-insns
          (add-monitors
           (rewrite-instructions* all-rules insns)))
         (prog-entry-label (gensym))
         ;; note that "extract-register-names" removes pc and flag registers
         ;; for pc register, we don't need to store it,
         ;; but we need to keep flag register
         ;; as gc routine might use it. (TODO: now gc-flag keeps it)
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
           (root-preallocator (length user-registers))))
         ;; instructions to save registers to root
         ;; TODO: should we expand instructions in these
         ;; instruction generating functions?
         (save-registers-insns
          (rewrite-instructions*
           all-rules
           (save-registers-to-root user-registers)))
         ;; instructions to restore registers from root
         (restore-registers-insns
          (rewrite-instructions*
           all-rules
           (restore-registers-from-root user-registers)))
         )
    `(;; initialization code here
      ,@root-preallocate-insns
      (goto (label ,prog-entry-label))
      ;; subroutines here
      gc-maybe-start
      (assign gc-flag (reg flag))
      ;; if free = memory size, we need gc
      (assign gc-temp (op to-pointer) (const ,machine-memory-size))
      (test (op ptr=?) (reg free) (reg gc-temp))
      (branch (label gc-needed))
      (goto (label gc-all-done))
      gc-needed
      (perform (op debug-gc-start))
      ;; TODO: for now gc is never triggered
      ;; TODO: just save and restore to see if it works
      ,@save-registers-insns
      ,@gc-code
      ,@restore-registers-insns
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

