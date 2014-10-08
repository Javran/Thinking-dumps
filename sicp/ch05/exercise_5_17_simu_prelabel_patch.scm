(define (assemble insns machine)
  (define (make-instruction insn-text prev-text)
    (if (symbol? insn-text)
        insn-text
        ;; else create the instruction representation
        ;; (list <text> <proc> <label / #f>)
        (list insn-text
              (make-execution-procedure insn-text machine)
              (if (symbol? prev-text)
                  prev-text
                  #f))))
  (define (drop-labels insns)
    (filter (compose not symbol?) insns))

  (define (scan-duplicate-labels insns label?)
    (let* ((labels (filter label? insns))
           (dup-lbl (first-dup-element labels)))
      (if dup-lbl
          (error "multiple labels with the same name:"
                 (car dup-lbl))
          'ok)))

  (scan-duplicate-labels insns symbol?)
  (let ((insns
         ;; zip the instruction list with one offset,
         ;; so that the previous instruction can be seen by the following one
         ;; e.g. (map p '(a b c) '(#f a b c))
         ;;   => '((p a #f) (p b a) (p c b))
         (map make-instruction
              insns (cons #f insns))))
    (let ((jump-table
           (let loop ((table '())
                      (insns insns))
             (if (null? insns)
                 table
                 (let ((hd (car insns))
                       (tl (cdr insns)))
                   (if (symbol? hd)
                       (loop (cons (list hd (drop-labels tl))
                                   table)
                             tl)
                       (loop table
                             tl)))))))
      (machine-set-instruction-sequence! machine (drop-labels insns))
      (machine-set-jump-table! machine jump-table))))

(define assembled-insn-proc cadr)
(define assembled-insn-prev-label caddr)
