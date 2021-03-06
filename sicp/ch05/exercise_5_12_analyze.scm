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

;; a list (without duplicates) of the registers used
;; to hold entry points
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
   (concat-map extract insns)))

;; a list (without duplicates) of the registeres
;; that are saved or restored
(define (saved-or-restored-regs insns)
  (define (extract insn)
    (if (and (non-empty? insn)
             (or (eq? (car insn) 'save)
                 (eq? (car insn) 'restore)))
        (list (cadr insn))
        '()))
  (remove-duplicates
   (concat-map extract insns)))

;; for each register, a list (without duplicates)
;; of the sources from which it is assigned
(define (assign-sources insns)
  (define (assign? insn)
    (and (non-empty? insn)
         (eq? (car insn) 'assign)))
  ;; add new source into an existing reg-srcs alist
  (define (add-new-source key new-src alist)
    (let ((srcs (cdr (assoc key alist))))
      (cons (cons key (cons new-src srcs))
            (del-assq key alist))))
  (let* ((assigns (filter assign? insns))
         (targets (remove-duplicates
                   (map cadr assigns)))
         (source-alist
          ;; create a map between key and value (a list of source)
          (map (lambda (k)
                 (cons k '()))
               targets))
         (source-alist-1
          ;; go though the interesting instructions
          ;; accumulate sources into an alist
          ;; there might be duplicates in the value of alist
          (let loop ((xs assigns)
                     (result source-alist))
            (if (null? xs)
                result
                (let* ((x (car xs))
                       (k (cadr x))
                       (v (cddr x)))
                  (loop (cdr xs)
                        (add-new-source k v result))))))
         (source-alist-2
          ;; remove duplicates and sort in the "cdr" part
          (map (lambda (p)
                 (cons
                  (car p)
                  (sort (remove-duplicates (cdr p))
                        to-str-compare)))
               source-alist-1)))
    source-alist-2))

(define (data-path-analyze insns)
  `((sui . ,(sorted-uniq-instructions insns))
    (epr . ,(entry-point-regs insns))
    (srr . ,(saved-or-restored-regs insns))
    (as  . ,(assign-sources insns))))

(define (pretty-print-data-path-analysis result)
  (define (find k)
    (cdr (assoc k result)))
  (let ((sui (find 'sui))
        (epr (find 'epr))
        (srr (find 'srr))
        (as  (find 'as )))
    (out "# Data path analysis result")

    (out "* Instructions:")
    (for-each out sui)

    (out "* Entry Point Holding Registers:")
    (for-each out epr)

    (out "* Saved or Restored Registers:")
    (for-each out srr)

    (out "* Register Assigning Sources:")
    (for-each
     (lambda (p)
       (format #t "~A:~%" (car p))
       (for-each
        (lambda (x)
          (format #t "  ~A~%" x))
        (cdr p)))
     as)
    'done))
