(load "./exercise_5_16_simu_tracing_patch.scm")

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

;; redo ex 5.16 patch here to apply new changes
(define (machine-execute! m)
  (let ((insns (machine-reg-get m 'pc)))
    (if (null? insns)
        'done
        (let* ((insn (car insns))
               (proc (assembled-insn-proc insn))
               (text (assembled-insn-text insn))
               (lbl  (assembled-insn-prev-label insn)))
          ;; print tracing message before
          ;; the instruction gets executed
          (if (machine-trace? m)
              (begin
                (if lbl
                    (format #t "into label: ~A~%" lbl)
                    'skipped)
                (out text))
              'skipped)
          (proc)
          (machine-execute! m)))))
