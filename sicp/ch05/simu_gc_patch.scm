(load "./simu_lower_patch.scm")
(load "./gc-code.scm")

(load "./gc-registers.scm")

;; registers that shouldn't be stored by "root" register
(define machine-reserved-gc-registers
  (cons 'gc-resume-point
        (extract-register-names gc-code)))

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
         ;; as gc routine might use it.
         (user-registers
          (remove-duplicates
           (set-diff (cons 'flag (extract-register-names expanded-insns))
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
      ;; TODO: for now gc is never triggered
      ;; TODO: just save and restore to see if it works
      ,@save-registers-insns
      ,@restore-registers-insns
      (goto (reg gc-resume-point))
      ;; program entry
      ,prog-entry-label
      ,@expanded-insns)))

;; Local variables:
;; proc-entry: "./gc-test-machine.scm"
;; End:

