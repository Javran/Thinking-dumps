;; extract register names (except for "pc" and "flag")
;; from the controller text
;; only target registers in "assign" instructions and "restore" instructions
;; are taken into account
;; but this is sufficient, since no other instructions can modify a register
;; "pc" and "flag" are always removed from the result
(define (extract-register-names controller-text)
  (define (extract insn)
    (cond
     ((or (tagged-list? insn 'assign)
          (tagged-list? insn 'restore))
      (list (cadr insn)))
     (else '())))
  (remove-duplicates
   (set-diff
    (apply append (map extract controller-text))
    '(pc flag))))

;; build a machine with controller-text assembled,
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
  (let ((m (empty-machine))
        (reg-names-1 (extract-register-names controller-text))
        (reg-names-2 (map car init-reg-table)))
    (let ((reg-names (sort
                      (remove-duplicates
                       (set-diff
                        (apply append (list reg-names-1 reg-names-2))
                        '(pc flag)))
                      (lambda (x y)
                        (string<=? (symbol->string x)
                                   (symbol->string y))))))
      ;; initialize registers
      (machine-define-registers! m reg-names)
      (for-each
       (lambda (pair)
         (machine-reg-set! m (car pair) (cadr pair)))
       init-reg-table)
      ;; primitive operation table setup
      (machine-set-operations! m (ops-builder m))
      ;; assemble
      (assemble controller-text m)
      ;; start execution
      (machine-reset-pc! m)
      (machine-execute! m)
      m)))

(define default-ops-buidler
  (lambda (m)
    `( (+ ,+)
       (- ,-)
       (* ,*)
       (/ ,/)
       (zero? ,zero?)
       )))

(define (build-and-execute controller-text)
  (build-and-execute-with controller-text '() default-ops-buidler))
