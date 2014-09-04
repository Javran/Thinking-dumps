(load "./simu_machine.scm")
(load "./simu_assemble_handlers.scm")

(define (assemble insns machine)
  ;; expected input: a list of instructions (including labels)
  ;; we don't have to do all the things in one pass
  ;; as this won't be the bottleneck of the whole program
  ;; we also don't need to perform the continuation passing trick
  ;;
  ;; to be clear with terms:
  ;; instruction text is what the instruction looks like in controller text
  ;; (e.g. (assign x (const 1)))
  ;; instruction execution procedure is a procedure without arguments.
  ;; it performs the operation described by the instruction text when executed.
  ;; instruction is a pair consisted of instruction text and instruction-exec-proc
  ;;
  ;; in the first pass, we simply turn instruction text into
  ;; a pair: (<instruction-text> . <instruction-execution-procedure>)

  ;; and in the second pass, we make the label-instruction alist
  (define (make-instruction insn-text)
    (if (symbol? insn-text)
        ;; labels are kept as it is when making instructions
        insn-text
        ;; deal with real instructions
        (cons insn-text
              (make-execution-procedure insn-text machine))))

  ;; despite that the original program does not consider this issue,
  ;; but it's entirely possible that two labels can point to the same location
  ;; which might result in some labels being accidentally included in
  ;; the instruction list. (e.g. instruction-text list:
  ;; (lbl1 lbl2 (assign foo (reg bar))) => ( (lbl1 (lbl2 (assign ...)))
  ;;                                         (lbl2 (assign ...)) )
  ;; while we really want: ( (lbl1 (assign ...))
  ;;                         (lbl2 (assign ...)) )
  ;; ).
  ;; here we drop all the labels to get the expected result
  (define (drop-labels insns)
    (filter (compose not symbol?) insns))

  (let ((insns (map make-instruction insns)))
    (let ((jump-table
           (let loop ((table '())
                      (insns insns))
             (if (null? insns)
                 table
                 (let ((hd (car insns))
                       (tl (cdr insns)))
                   (if (symbol? hd)
                       ;; label detected
                       (loop (cons (list hd (drop-labels tl))
                                   table)
                             tl)
                       (loop table
                             tl)))))))
      (machine-set-instruction-sequence! machine (drop-labels insns))
      (machine-set-jump-table! machine jump-table))))

;; Local variables:
;; proc-entry: "./simu.scm"
;; End:
