(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")

;; TODO: the modification will be based on
;; "simu.scm" but later dependencies will be minimized
;; so that something can be shared between different simulators

(load "./figure_5_12.scm")

;; just comparing their string output will do
(define (to-str-compare a b)
  (string<=? (format #f "~A" a)
             (format #f "~A" b)))

;; well, we agree on one thing: labels are symbols.
(define (remove-labels insns)
  (filter (compose not symbol?) insns))

;; a list of all instructions, with duplicates removed,
;; sorted by instruction type.
(define (sorted-uniq-instructions insns)
  (sort
   (remove-duplicates
    (remove-labels insns))
   to-str-compare))

(define (entry-point-regs insns)
  (define (extract insn)
    (if (and (non-empty? insn)
             ;; (goto (???))
             (eq? (car insn) 'goto)
             ;; (goto (reg ???))
             (eq? (caadr insn) 'reg))
        (list (cadr (cadr insn)))
        '()))
  (remove-duplicates
   (concat
    (map extract insns))))

(for-each out (entry-point-regs (cdr fib-machine-controller)))

(end-script)
