;; we are trying to generate instruction lists
;; that maintains register values here.

;; TODO: might be a temporary module, might be merged into
;; other modules when the impl is done
(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./rewrite.scm")
(load "./rewrite-instructions.scm")
(load "./list-stack-rewrites.scm")

;; TODO: initialize "root" register before gc starts
;; TODO: update registers according to "root" register
;; after the gc is done
;; TODO: we might need to generate some code everytime after
;; we increase "free", as we need to test if it exceeds the limit
;; and start doing garbage collection immediately
;; and also before jumping to the garbage collection subroutine,
;; we need to record the current location so that we can jump back
;; when garbage collection is done, I guess this can be done
;; by generating unique symbols before we start to assemble the code

;; generate an instruction list that pre-allocates
;; a list for storing all registers that are not
;; related to the garbage collecting algorithm.
;; this generated list should be executed when the machine starts
;; reg-count: the length of the list that
;; we are going to pre-allocate
(define (gen-insn-root-preallocator reg-count)
  `(pre-allocate-root-list
    ,@(tree->instruction-list
       ;; the value doesn't matter, we will fill it right before
       ;; gc takes place
       (replicate reg-count 0))
    (assign root (reg result))))

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

;; attach some extra code after every time free pointer gets increased.
;; TODO: better to be an internal function
;; use "gc-resume-point" to resume back to the next instruction
;; a subroutine "gc-maybe-start" check if we are running out of space
;; and determine if we need to perform gc right now,
;; then that subroutine needs to jump back using the label
;; stored in "gc-resume-point"
(define (gen-insn-add-monitors insns)
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
