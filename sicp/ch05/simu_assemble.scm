(load "simu_machine.scm")
(load "simu_assemble_handlers.scm")

;; note that for "assemble" procedure,
;; there is one important difference between our implementation
;; and the one in the book (which I usually refer to as "the legacy one"):
;; * the legacy "assemble" procedure is additive,
;;   in a sense that if you try to assemble instruction sequences
;;   multiple times in a machine, all of them will get concatenated together
;; * our implementation makes the instruction sequence tie to the machine,
;;   we have to pass the whole instruction sequence when doing assembling,
;;   and if we assemble instruction sequences multiple times,
;;   only the last one will be installed.
;;   however, this is not a big problem: under most circumstances
;;   we are able to figure out the whole instruction sequence
;;   before start to execute the code
(define (assemble insns machine)
  ;; expected input: a list of instructions (including labels)
  ;; we don't have to do all the things in one pass
  ;; as this won't be the bottleneck of the whole program
  ;; we also don't have to perform the continuation passing trick
  ;; for the same reason
  ;;
  ;; to be clear with terms:
  ;; instruction text is what the instruction looks like in controller text
  ;; (e.g. (assign x (const 1)))
  ;; instruction execution procedure is a procedure without arguments.
  ;; it performs the operation described by the instruction text when executed.
  ;; instruction is a pair consisted of instruction text and instruction-exec-proc

  ;; in the first pass, if there are multiple labels with the same name
  ;; an error will be raised immediately
  ;;
  ;; in the second pass, we simply turn instruction text into
  ;; a pair: (<instruction-text> . <instruction-execution-procedure>)

  ;; and in the third pass, we make the label-instruction alist
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

  (define (scan-duplicate-labels insns label?)
    (let* ((labels (filter label? insns))
           (dup-lbl (first-dup-element labels)))
      (if dup-lbl
          ;; here we can even report all the labels with
          ;; the same name, but I find it not very useful.
          (error "multiple labels with the same name:"
                 (car dup-lbl))
          'ok)))

  ;; enforcing each label to be unique
  (scan-duplicate-labels insns symbol?)
  (let ((insns (map make-instruction insns)))
    (let ((jump-table
           ;; traverse the assembled instructions
           ;; to collect labels to build jump-table
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
