(load "./compiler-list.scm")

;; functions related to instruction sequences

;; the instruction sequence is also keeping some extra
;; information to avoid doing redundant analyzing work
;;
;; - need: the set of registers must be initialized before
;;     execution
;; - modifies: the set of registers whose value are modified
;;     by the instruction sequence
;; - statements: the instruction sequence
;;
;; note that a symbol (label) is considered a degenerated case
;; of an instruction sequence

(define (make-instruction-sequence
         needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

;; instruction sequences' equality
(define (instruction-sequence-eq? seq1 seq2)
  (and (set-equal? (registers-needed seq1)
                   (registers-needed seq2))
       (set-equal? (registers-modified seq1)
                   (registers-modified seq2))
       (equal? (statements seq1)
               (statements seq2))))

;; accessors for the instruction-sequence structure
(define (registers-needed s)
  (if (symbol? s)
      '()
      (car s)))
(define (registers-modified s)
  (if (symbol? s)
      '()
      (cadr s)))
(define (statements s)
  (if (symbol? s)
      (list s)
      (caddr s)))

;; whether a given sequence needs or modifies
;; a given register
(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

;; appending arbitrary number of instruction sequences together
(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     ;; registers needed is the set of registers needed by seq1
     ;; and the sef of registers needed by seq2 without those
     ;; that get initialized by seq1
     (list-union
      (registers-needed seq1)
      (list-difference (registers-needed seq2)
                       (registers-modified seq1)))
     ;; registers modified is the set of registers
     ;; modified by either of them
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     ;; statements are appended together
     (append (statements seq1)
             (statements seq2))))
  (fold-right
   append-2-sequences
   (empty-instruction-sequence)
   seqs))

;; merge two instruction sequences preserving
;; some registers specified by "regs"
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            ;; apply protection only if a register is needed
            ;; by seq2 but modified by seq1
            (preserving
             (cdr regs)
             (make-instruction-sequence
              ;; register pulled in by "(save <reg>)"
              (list-union (list first-reg)
                          (registers-needed seq1))
              ;; register whose modification is cancelled by
              ;; "(save <reg>)" and "(restore <reg>)"
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            ;; else
            (preserving (cdr regs) seq1 seq2)))))

;; attach a non-inline instruction sequence (maybe a procedure body)
;; to another one
(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq)
           (statements body-seq))))

;; merge instruction sequences together,
;; two branches will never be executed sequentially
(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1)
           (statements seq2))))

;; pretty print an instrcution sequence structure
(define (print-instruction-sequence insn-seq)
  (format #t "Registers needed: ~A~%~
              Registers modified: ~A~%~
              ;;;; Instruction listing:~%"
          (registers-needed insn-seq)
          (registers-modified insn-seq))
  (for-each (lambda (insn)
              (format #t "  ~A~%" insn))
            (statements insn-seq))
  (out ";;;; End of listing"))

(define (compiler-insn-seq-tests)
  (begin
    (define insn-seq-1
      (make-instruction-sequence
       '(a b c)
       '(c d)
       ;; the instruction sequence does not matter
       ;; as we only care about its ordering here
       '(seq-1-1 seq-1-2 seq-1-3)))
    (define insn-seq-2
      (make-instruction-sequence
       '(c d)
       '(a d)
       '(seq-2-1 seq-2-2 seq-2-3)))
    (define insn-seq-3
      (make-instruction-sequence
       '()
       '(a b c)
       '(seq-3-1 seq-3-2 seq-3-3)))

    ;; test "append-instruction-sequences"
    (do-test
     append-instruction-sequences
     (list
      (mat (empty-instruction-sequence)
           (empty-instruction-sequence))
      (mat (empty-instruction-sequence) insn-seq-1
           insn-seq-1)
      (mat insn-seq-1 insn-seq-2 insn-seq-3
           (make-instruction-sequence
            ;; [a,b,c] + ([c,d]-[c,d]) + ([] - [a,d]) = [a,b,c]
            '(a b c)
            ;; [c,d] + [a,d] + [a,b,c] = [a,b,c,d]
            '(a b c d)
            '(seq-1-1 seq-1-2 seq-1-3
              seq-2-1 seq-2-2 seq-2-3
              seq-3-1 seq-3-2 seq-3-3)))
      (mat insn-seq-3 insn-seq-1 insn-seq-2
           (make-instruction-sequence
            ;; [] + ([a,b,c]-[a,b,c]) + ([c,d]-[c,d]) = []
            '()
            ;; [a,b,c] + [c,d] + [a,d] = [a,b,c,d]
            '(a b c d)
            '(seq-3-1 seq-3-2 seq-3-3
              seq-1-1 seq-1-2 seq-1-3
              seq-2-1 seq-2-2 seq-2-3))))
     instruction-sequence-eq?)

    ;; test "preserving"
    (do-test
     preserving
     (list
      (mat '(a b c d) (empty-instruction-sequence) insn-seq-1
           insn-seq-1)
      (mat '(a b c d) insn-seq-3 (empty-instruction-sequence)
           insn-seq-3)
      ;; no need to preserve "a"
      (mat '(a) insn-seq-1 insn-seq-2
           (append-instruction-sequences insn-seq-1 insn-seq-2))
      (mat '(c) insn-seq-1 insn-seq-2
           (make-instruction-sequence
            '(a b c)
            '(a d)
            '(;; "c" should be preserved
              (save c)
              seq-1-1 seq-1-2 seq-1-3
              (restore c)
              seq-2-1 seq-2-2 seq-2-3)))
      (mat '(d c) insn-seq-1 insn-seq-2
           (make-instruction-sequence
            ;; "(save d)" wil pull in "d"'s requirement
            '(a b c d)
            ;; "d" modified by seq2
            '(a d)
            '(;; both "c" and "d" should be preserved
              ;; but actually the order does not matter,
              ;; could be (save d) (save c) ... (restore c) (restore d)
              (save c)
              (save d)
              seq-1-1 seq-1-2 seq-1-3
              (restore d)
              (restore c)
              seq-2-1 seq-2-2 seq-2-3)))
      (mat '(a b c) insn-seq-1 insn-seq-3
           ;; nothing need to be preserved
           (append-instruction-sequences insn-seq-1 insn-seq-3))
      (mat '(a b c d) insn-seq-3 insn-seq-2
           (make-instruction-sequence
            '(c d)
            '(a b d)
            '((save c)
              seq-3-1 seq-3-2 seq-3-3
              (restore c)
              seq-2-1 seq-2-2 seq-2-3))))
     instruction-sequence-eq?)
    ;; end of tests
    ))

;; Local variables:
;; proc-entry: "./compiler-tests.scm"
;; End:
