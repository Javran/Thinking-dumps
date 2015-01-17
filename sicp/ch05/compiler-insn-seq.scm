(load "./compiler-list.scm")
;; functions about instruction sequences

;; note that a symbol (label) is considered a degenerated case
;; of an instruction sequence

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
  ;; TODO: this is a fold
  #;(define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences
         (car seqs)
         (append-seq-list (cdr seqs)))))
  #;(append-seq-list seqs)
  (fold-right
   append-2-sequences
   (empty-instruction-sequence)
   seqs))



(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving
             (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq)
           (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1)
           (statements seq2))))

;; TODO: break functions into modules,
;; need to figure out them in the reversed way
