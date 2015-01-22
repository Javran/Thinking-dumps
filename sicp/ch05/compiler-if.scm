;; compile if-expressions
(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           ;; for true branch:
           ;; after its evluation, we need to jump
           ;; to the end if the linkage is "'next"
           ;; to prevent executing the false branch.
           (if (eq? linkage 'next)
               after-if
               linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code (compile (if-consequent exp)
                             target
                             consequent-linkage))
            (a-code (compile (if-alternative exp)
                             target
                             linkage)))
        (preserving
         '(env continue)
         ;; evaluate precondition
         p-code
         (append-instruction-sequences
          (make-instruction-sequence
           '(val) '()
           ;; choose branch according to
           ;; the value of "val"
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          ;; true branch and false branch in parallel
          ;; because "append-instruction-sequences"
          ;; will take care of the modified/needed register set
          ;; we need to put these two branches
          ;; in "parallel-instruction-sequences" first
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))
