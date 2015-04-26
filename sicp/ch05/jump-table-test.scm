(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; this module contains a more space-efficient
;; implementation of building jump-tables.
;; in the original implementation of my "simu.scm",
;; the naive approach is used. It works fine
;; when the number of instructions is not too large.
;; but when the number of instructions goes up,
;; there are many spaces storing duplicated informations
;; and eventually runs out of memory,
;; in a modern machine, it might be possible to solve it
;; by increasing the allowed memory upper bound,
;; but that solution does not sound good.
;; this module tries to deal with the problem
;; by enabliing more sharing between jump-table cells,
;; hopefully the result will be more space-efficient.

;; module assumptions:
;;
;; * instructions are either label or non-label
;;   this can be examined by "label?" function
;; * every label is unique

(define label? symbol?)

;; TODO: need a document talking about the implementation details
;; I guess it will be more clean to have an individual doc
(define (build-jump-table origin-insns)
  ;; define internal state to be
  ;; (list <insns> <no-lbl-insns>)
  ;; insns: the original list of instructions
  ;;   with some initial instructions dropped.
  ;; no-lbl-insns: the instruction list
  ;;   with all the labels removed.
  ;; INVARIANT: two lists should be synchronizing
  ;;   with each other: if we removed labels from insns,
  ;;   the resulting list should exactly be no-lbl-insns

  (define (sync-insn-skip state)
    ;; the purpose for "sync-insn-skip" is to skip matching
    ;; instructions, and get to a state where the head of "insns"
    ;; is a label, and the head of "no-lbl-insns" is pointing to
    ;; the place where the label is supposed to point to.
    ;; INVARIANTS:
    ;; * state can never be (list '() '())
    ;; * this function will always return a state
    ;;   where the head of "insns" is a label
    ;;   and "no-lbl-insns" points to the list of instructions
    ;;   that "label" leads to.
    (assert (not (equal? (list '() '())
                         state))
            "invalid input state to sync-insn-skip")
    ;; * the first non-label instruction should
    ;;   exactly be the first instruction in no-lbl-insns
    ;; (if the state invariant is satisfied, this invariant
    ;;  is guaranteed to be followed)
    (let ((insns        (car  state))
          (no-lbl-insns (cadr state)))
      (let ((hd1 (car insns))
            (tl1 (cdr insns)))
        ;; NOTE: it is possible that no-lbl-insns have no element
        ;; but insns has something
        ;; (should be all labels in this case)
        (if (label? hd1)
            state
            ;; hd1 is an instruction
            (let ((hd2 (car no-lbl-insns))
                  (tl2 (cdr no-lbl-insns)))
              (begin
                (assert
                 (and
                  ;; hd2 should better be an instruction
                  (not (label? hd2))
                  ;; and hd1 should be exactly the same as hd2
                  (equal? hd1 hd2))
                 "state out of sync")
                ;; time to proceed
                (sync-insn-skip (list tl1 tl2))))))))

  (define (sync-next-label lbl state)
    ;; first we "normalize" the state
    ;; by skipping instructions in both lists
    ;; until "insns" is a label and "no-lbl-insns"
    ;; has the corresponding elements
    ;; INVARIANT:
    ;; * "lbl" should be the next label available in
    ;;   current "insns"
    (let ((state (sync-insn-skip state)))
      (let ((insns (car state))
            (no-lbl-insns (cadr state)))
        (assert (eq? lbl (car insns)))
        ;; first one must be a label
        (list (list lbl no-lbl-insns)
              ;; new state
              (list (cdr insns) no-lbl-insns)))))

  ;; TODO: synchonized search against two lists
  ;; TODO: synchonized until left side is a label
  ;; make the binding, return new state
  ;; keep going until all labels are consumed
  ;; TODO: demonstrate why it works
  (let ((collected
         ;; collect labels and instructions in order
         (let loop ((labels '())
                    (no-lbl-insns '())
                    (insns origin-insns))
           (if (null? insns)
               ;; because the elements are accumulated
               ;; from left to right, so we need to
               ;; reverse them when we are done
               ;; we are using "reverse!" to save space
               ;; and increase efficiency.
               ;; since we are not using the result anywhere
               ;; else, this destructive version is safe.
               (cons (reverse! labels)
                     (reverse! no-lbl-insns))
               (let ((hd (car insns))
                     (tl (cdr insns)))
                 (if (label? hd)
                     (loop (cons hd labels)
                           no-lbl-insns
                           tl)
                     (loop labels
                           (cons hd no-lbl-insns)
                           tl)))))))
    (let ((labels (car collected))
          (no-lbl-insns (cdr collected)))
      (let loop ((labels labels)
                 (state (list origin-insns
                              no-lbl-insns))
                 (jump-table '()))
        (if (null? labels)
            jump-table
            (let ((hd (car labels))
                  (tl (cdr labels)))
              (let ((result (sync-next-label hd state)))
                (let ((entity (car result))
                      (new-state (cadr result)))
                  (loop tl new-state (cons entity jump-table))))))))))

(for-each
 out
 (build-jump-table
  '(a
    (a b)
    b
    c
    d
    (c d e)
    (e)
    (f)
    g
    (h)
    (i j k)
    l
    (j k)
    m
    (m n)
    i)))

(end-script)
