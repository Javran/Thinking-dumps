;; extract all register names
;; from the list of instructions
;; only target registers in "assign" instructions and "restore" instructions
;; are taken into account
;; but this is sufficient, since no other instructions can modify a register
(define (extract-register-names insns)
  (define (extract insn)
    (cond
     ((or (tagged-list? insn 'assign)
          (tagged-list? insn 'restore))
      (list (cadr insn)))
     (else '())))
  (remove-duplicates
   (apply append (map extract insns))))

;; build a machine with insns assembled,
;; registers assigned according to the table,
;; and primitive operations specified
;;
;; we limit the set of registers
;; to only those that has appeared in the instruction list.
;;
;; Previously I want to make it flexible so that I can plug in
;; instruction list to the machine as well.
;; But why not just making another machine instead of
;; reusing the old one if the instruction list get completely changed?
(define (build-with
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
         (reg-names (extract-register-names insns)))
    (machine-define-registers! m reg-names)
    ;; 'pc and 'flag might not appear in `reg-names`
    ;; but it is guaranteed that they will be defined after
    ;; the call to "machine-define-registers!"
    (for-each
     (lambda (pair)
       ;; no need to check if the register exists
       ;; setting values to any undefined regster results
       ;; in a register not found error
       (machine-reg-set! m (car pair) (cadr pair)))
     init-reg-table)
    ;; primitive operation table setup
    (machine-set-operations! m (ops-builder m))
    ;; assemble
    (assemble insns m)
    m))

;; make a machine from controller text
;; and operation list builder
;; with all registers uninitialized
(define (ctl-ops->machine
         controller-text
         ops-builder)
  (let* ((insns (cdr controller-text))
         (m (empty-machine))
         (reg-names (extract-register-names insns)))
    (machine-define-registers! m reg-names)
    ;; 'pc and 'flag might not appear in `reg-names`
    ;; but it is guaranteed that they will be defined after
    ;; the call to "machine-define-registers!"

    ;; primitive operation table setup
    (machine-set-operations! m (ops-builder m))
    ;; assemble
    (assemble insns m)
    m))

;; initialize registers
(define (machine-init-regs
         m init-reg-table)
  ;; clean up all values
  (for-each
   (lambda (pair)
     (machine-reg-set! m (car pair) '*unassigned*))
   (map car machine-register-table))

  ;; set values
  (for-each
   (lambda (pair)
     ;; no need to check if the register exists
     ;; setting values to any undefined regster results
     ;; in a register not found error
     (machine-reg-set! m (car pair) (cadr pair)))
   init-reg-table))

;; build it and execute it
(define (build-and-execute-with
         controller-text
         init-reg-table
         ops-builder)
  (let ((m (build-with
            controller-text
            init-reg-table
            ops-builder)))
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
       (= ,=)
       (square ,square)
       (abs ,abs)
       (average ,average)
       )))

(define (build-and-execute controller-text reg-bindings)
  (build-and-execute-with
   controller-text
   reg-bindings
   default-ops-buidler))
