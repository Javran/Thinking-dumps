;; extract register names (except for "pc" and "flag")
;; from the list of instructions
;; only target registers in "assign" instructions and "restore" instructions
;; are taken into account
;; but this is sufficient, since no other instructions can modify a register
;; "pc" and "flag" are always removed from the result
(define (extract-register-names insns)
  (define (extract insn)
    (cond
     ((or (tagged-list? insn 'assign)
          (tagged-list? insn 'restore))
      (list (cadr insn)))
     (else '())))
  (remove-duplicates
   (set-diff
    (apply append (map extract insns))
    '(pc flag))))

;; remove duplicates and 'pc & 'flag registers, sort
;; making it ready to use as a regular register name list
(define (merge-register-lists
         reg-names-1 reg-names-2)
  (sort
   (remove-duplicates
    (set-diff
     (apply append (list reg-names-1 reg-names-2))
     '(pc flag)))
   (lambda (x y)
     (string<=? (symbol->string x)
                (symbol->string y)))))

;; build a machine with insns assembled,
;; registers assigned according to the table,
;; and primitive operations specified
(define (build-and-execute-with
         ;; the controller text
         controller-text
         ;; an optional register table
         ;; (does not need to be a table containing all registers)
         init-reg-table
         ;; when given the machine itself,
         ;; produces a primitive-operation table
         ops-builder)
  (let* ((insns (cdr controller-text))
         (m (empty-machine))
         (reg-names-1 (extract-register-names insns))
         (reg-names-2 (map car init-reg-table))
         (reg-names (merge-register-lists reg-names-1 reg-names-2)))
    ;; initialize registers
    (machine-define-registers! m reg-names)
    (for-each
     (lambda (pair)
       (machine-reg-set! m (car pair) (cadr pair)))
     init-reg-table)
    ;; primitive operation table setup
    (machine-set-operations! m (ops-builder m))
    ;; assemble
    (assemble insns m)
    ;; start execution
    (machine-reset-pc! m)
    (machine-execute! m)
    m))

(define default-ops-buidler
  (lambda (m)
    `( (+ ,+)
       (- ,-)
       (* ,*)
       (/ ,/)
       (zero? ,zero?)
       (> ,>)
       (>= ,>=)
       (< ,<)
       (<= ,<=)
       (square ,square)
       (abs ,abs)
       (average ,average)
       )))

(define (build-and-execute controller-text reg-bindings)
  (build-and-execute-with
   controller-text
   reg-bindings
   default-ops-buidler))
