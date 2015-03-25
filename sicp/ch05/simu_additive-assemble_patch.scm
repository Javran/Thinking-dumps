;;; this module makes "assemble" procedure additive.
;;; so that the behavior of simu is closer to the legacy one

;; theoretically it's always safe to apply this patch whenever "simu.scm"
;; is imported as long as we don't use "assemble" procedure more than once
;; on a single machine.

(define (assemble insns machine)
  ;; unlike "assemble" in "simu.scm",
  ;; this "assemble" procedure accumulates assembled instructions.
  ;; and returns the assembled version of "insns" passed to it
  ;; (note that only the portion corresponding to "insns" are returned
  ;;  not the whole sequence of assembled instructions)
  ;; labels are also dropped from the assembled instructions
  ;; so this procedure produces a value that "continue" register
  ;; can take, even if the machine don't know how to handle labels
  ;; (NOTE: handling labels are easy however
  ;; -- just ignoring them and we are done)
  (define (make-instruction insn-text)
    (if (symbol? insn-text)
        insn-text
        (cons insn-text
              (make-execution-procedure insn-text machine))))

  (define (drop-labels insns)
    (filter (compose not symbol?) insns))

  ;; "scan-duplicate-labels" from the original implementation
  ;; is removed here but the check is done every time
  ;; new instruction sequence gets inserted.

  (let ((insns (map make-instruction insns)))
    (let ((new-jump-table
           (let loop ((table
                       ;; begin with the stored jump table
                       ;; so that we can refer to labels
                       ;; inserted last time.
                       (machine-jump-table machine))
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
                             tl))))))
          (new-insn-seq
           (append
            (machine-instruction-sequence machine)
            (drop-labels insns))))
      ;; duplicated label detection
      (let ((has-dup-element
             ;; check keys
             (first-dup-element
              (map car new-jump-table))))
        (if has-dup-element
            (error "multiple labels with the same name:"
                   (car has-dup-element))
            'ok))
      (machine-set-instruction-sequence!
       machine new-insn-seq)
      (machine-set-jump-table!
       machine new-jump-table)
      (drop-labels insns))))
