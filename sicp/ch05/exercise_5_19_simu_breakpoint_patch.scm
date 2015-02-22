(load "exercise_5_17_simu_prelabel_patch.scm")
(load "exercise_5_19_breakpoint_table.scm")

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

;; corresponding to "set-breakpoint" feature
;; requested by the exercise
(define (machine-set-breakpoint! m lbl n)
  (machine-extra-modify!
   m
   'breakpoint-table
   (lambda (tbl)
     (breakpoint-table-add lbl n tbl))
   '()))

;; corresponding to "cancel-breakpoint" feature
;; requested by the exercise
(define (machine-cancel-breakpoint! m lbl n)
  (machine-extra-modify!
   m
   'breakpoint-table
   (lambda (tbl)
     (breakpoint-table-del lbl n tbl))
   '()))

;; when machine gets resumed, the same lines of expressions
;; gets executed, and we need to make a difference between
;; reaching a breakpoint and resuming from a breakpoint
;; and this difference is made by this resuming flag
;; if the flag is set, the breakpoint will be bypassed
;; and the flag will be unset immediately
;; or otherwise the breakpoint takes effect
(define (machine-resuming-flag? m)
  (machine-extra-get m 'resuming-flag #f))
(define (machine-set-resumming-flag! m flag)
  (machine-extra-set! m 'resuming-flag flag))

;; corresponding to "cancel-all-breakpoints" feature
;; requested by the exercise
(define (machine-cancel-all-breakpoints! m)
  (machine-extra-set! m 'breakpoint-table '()))

;; check if there is a breakpoint in this place?
;; NOTE: we could have cached the breakpoint table
;; associated with a label when we hit that label
;; it might be helpful to improve the performance
;; but here we won't do it for simplicity
(define (machine-breakpoint? m lbl n)
  (breakpoint-table-check?
   lbl n
   (machine-extra-get m 'breakpoint-table '())))

;; based on ex 5.17 patch
(define (machine-execute! m)
  (let ((insns (machine-reg-get m 'pc)))
    (if (null? insns)
        'done
        (let* ((insn (car insns))
               (proc (assembled-insn-proc insn))
               (text (assembled-insn-text insn))
               (lbl  (assembled-insn-prev-label insn)))
          ;; once the label is updated,
          ;; we reset the after-label counter
          (if lbl
              (begin
                (machine-set-current-label! m lbl)
                (machine-reset-after-label-counter! m))
              'skipped)

          ;; NOTE: the instruction text is shown *BEFORE*
          ;; the instruction gets executed
          (if (machine-trace? m)
              (begin
                (if lbl
                    (format #t "into label: ~A~%" lbl)
                    'skipped)
                (out text))
              'skipped)
          (if (and
               (machine-breakpoint?
                m
                (machine-current-label m)
                ;; NOTE that the counter bumps
                ;; after the instruction gets executed
                ;; therefore to break on a specified line,
                ;; we need to plus one to the current
                ;; after-label counter
                (add1 (machine-after-label-counter m)))
               (not (machine-resuming-flag? m)))
              ;; need to break the execution here
              (out "<breakpoint reached>")
              ;; else keep going
              (begin
                (if (machine-resuming-flag? m)
                    (machine-set-resumming-flag! m #f)
                    'skipped)
                (proc)
                (machine-extra-modify!
                 m 'instruction-counter add1 0)
                ;; whenever an instruction is executed,
                ;; we increase the after-label counter
                (machine-inc-after-label-counter! m)
                (machine-execute! m)))))))

(define (machine-fresh-start! m)
  ;; clear resuming flag if any
  (machine-set-resumming-flag! m #f)
  (machine-reset-pc! m)
  (machine-execute! m))

(define (machine-proceed! m)
  (machine-set-resumming-flag! m #t)
  (machine-execute! m))
