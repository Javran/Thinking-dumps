(load "./exercise_5_17_simu_prelabel_patch.scm")

;; the current label is the latest label
;; found in an instruction
(define (machine-current-label m)
  (machine-extra-get m 'current-label #f))
(define (machine-set-current-label! m lbl)
  (machine-extra-set! m 'current-label lbl))

;; maintain an "after-label-counter"
;; to track how many instructions we've passed
;; after seeing a label
;; everytime a new label is seen,
;; this counter should be reset.
(define (machine-after-label-counter m)
  (machine-extra-get m 'after-label-counter 0))
(define (machine-inc-after-label-counter! m)
  (machine-extra-modify! m 'after-label-counter add1 0))
(define (machine-reset-after-label-counter! m)
  (machine-extra-set! m 'after-label-counter 0))

;; add breakpoint to bp-table
(define (breakpoint-table-add lbl offset tbl)
  (let ((offsets
         (let ((pair (assoc lbl tbl)))
           (if pair
               (cadr pair)
               '()))))
    `((,lbl (,offset ,@offsets))
      ,@(del-assoc lbl tbl))))

;; del breakpoint from bp-table
(define (breakpoint-table-del lbl offset tbl)
  (let ((result (assoc lbl tbl)))
    (if result
        `((,lbl ,(delete offset (cadr result)))
          ,@(del-assoc lbl tbl))
        ;; label not found, nothing to be done
        tbl)))

;; based on ex 5.17 patch
(define (machine-execute! m)
  (let ((insns (machine-reg-get m 'pc)))
    (if (null? insns)
        'done
        (let* ((insn (car insns))
               (proc (assembled-insn-proc insn))
               (text (assembled-insn-text insn))
               (lbl  (assembled-insn-prev-label insn)))
          (if (machine-trace? m)
              (begin
                (if lbl
                    (format #t "into label: ~A~%" lbl)
                    'skipped)
                (out text))
              'skipped)
          ;; once the label is updated,
          ;; we reset the after-label counter
          (if lbl
              (begin
                (machine-set-current-label! m lbl)
                (machine-reset-after-label-counter! m))
              'skipped)
          (proc)
          (machine-extra-modify!
           m 'instruction-counter add1 0)
          ;; whenever an instruction is executed,
          ;; we increase the after-label counter
          (machine-inc-after-label-counter! m)
          (machine-execute! m)))))
