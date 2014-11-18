;; we are trying to generate instruction lists
;; that maintains register values here.

;; TODO: might be a temporary module, might be merged into
;; other modules when the impl is done
(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./rewrite.scm")
(load "./rewrite-instructions.scm")
(load "./list-stack-rewrites.scm")

;; duplicate value v to form a list of n elements
(define (replicate n v)
  (if (<= n 0)
      '()
      (cons v (replicate (sub1 n) v))))

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
        `( (perform (op set-car!) (reg result) (reg ,(car regs)))
           (assign result (op cdr) (reg result))
           ,@(save-regs-intern (cdr regs))
           )))
  `(save-registers-to-root
    (assign result (reg root))
    ,@(save-regs-intern regs)))

(define (restore-registers-from-root regs)
  (define (restore-regs-intern regs)
    (if (null? regs)
        '()
        `( (assign ,(car regs) (op car) (reg result))
           (assign result (op cdr) (reg result))
           ,@(restore-regs-intern (cdr regs))
           )))
  `(restore-registers-from-root
    (assign result (reg root))
    ,@(restore-regs-intern regs)))

(for-each out (restore-registers-from-root '(a b c)))
