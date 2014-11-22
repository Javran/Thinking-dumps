;; we are trying to generate instruction lists
;; that maintains register values here.

;; TODO: might be a temporary module, might be merged into
;; other modules when the impl is done
(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./rewrite.scm")
(load "./rewrite-instructions.scm")
(load "./list-stack-rewrites.scm")

;; generate an instruction list that allocates
;; a list for storing all registers that are not
;; related to the garbage collecting algorithm.
;; this generated list should be executed when the machine starts
;; reg-count: the length of the list that
;; we are going to pre-allocate
(define (root-preallocator reg-count)
  `(pre-allocate-root-list
    ,@(tree->instruction-list
       ;; the value doesn't matter, we will fill it right before
       ;; gc takes place
       (replicate reg-count 0))
    (assign root (reg result))))

(define (save-registers-to-root regs)
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

(define (restore-registers-from-root regs)
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

;; attach some extra code after every time free pointer gets increased.
;; TODO: better to be an internal function
;; use "gc-resume-point" to resume back to the next instruction
;; a subroutine "gc-maybe-start" check if we are running out of space
;; and determine if we need to perform gc right now,
;; then that subroutine needs to jump back using the label
;; stored in "gc-resume-point"
(define (add-monitors insns)
  (define (expand-insn insn)
    (if (equal? insn '(assign free (op ptr-inc) (reg free)))
        (let ((jump-back-label (gensym)))
          `((assign free (op ptr-inc) (reg free))
            (assign gc-resume-point (label ,jump-back-label))
            (goto (label gc-maybe-start))
            ,jump-back-label
            ))
        (list insn)))
  (concat-map expand-insn insns))
