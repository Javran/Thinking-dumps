(load "set.scm")

;; procedures:
;; * build without initialize registers
;;   (machine-init-regs! and machine-fresh-start!
;;    need to be called manually, these procdures are usually
;;    used when it is necessary to restart the machine with different
;;    register value settings)
;;   * ctl-ops->machine
;;
;; * build with an optional register inital value table
;;   (does everything)
;;   * build-with
;;   * build-and-execute-with
;;   * build-and-execute

;; extract all register names
;; from the list of instructions
;; all registers mentioned in the controller need to be taken into account
;; since now the register list fully depends on the controller text
;; if some registers are not present, we might have troube initializing
;; their values.
(define (extract-register-names insns)
  (define (extract insn)
    (if (symbol? insn)
        '()
        (let ((names1
               ;; special targets that does not have an "reg"
               ;; explicitly
               (cond
                ((or (tagged-list? insn 'assign)
                     (tagged-list? insn 'save)
                     (tagged-list? insn 'restore))
                 (list (cadr insn)))
                (else '())))
              (names2
               ;; registers indicated by "reg"
               (map cadr
                    (filter (lambda (e)
                              (and (list? e)
                                   (eq? 'reg (car e))))
                            insn))))
          (append names1 names2))))
  (remove-duplicates
   (concat-map extract insns)))

;; patches should modify this function
;; to implement instruction list transformations
;; e.g. rewriting some instructions to use a lower level implementation;
;; inserting some special implementations like garbage collection into
;; the instruction list, etc.
(define machine-do-insn-list-preprocess
  identity)

;; make a machine from controller text
;; and operation list builder
;; with all registers uninitialized
;; use `machine-init-regs! to [re-] initialize register values
;; and then `machine-fresh-start!` to [re-] start machine execution
(define (ctl-ops->machine
         controller-text
         ops-builder)
  (let* ((origin-insns (cdr controller-text))
         (insns (machine-do-insn-list-preprocess origin-insns))
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

;; There was a function called "ctl->machine"
;; which is the same as "ctl-ops->machine"
;; with "ops-builder" set to "default-ops-builder"
;; I removed it because it has rare usage.
;; More importantly, we want to make sure the usage of "default-ops-builder"
;; being limited in this module, or otherwise people like
;; me would start to think it's more convenient to overwrite "default-ops-builder"
;; rather than rewriting multiple functions to do their job.
;; The design should be:
;; * provide ONLY ONE interface that uses default value
;;   and leave other functions as flexible as possible.
;; * the default value / config should be
;;   used for users to bootstrap their customization,
;;   rather than being rewritten.

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
  (let ((m (ctl-ops->machine controller-text ops-builder)))
    (machine-init-regs! m init-reg-table)
    m))

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
    (machine-fresh-start! m)
    m))

(define (build-and-execute controller-text reg-bindings)
  (build-and-execute-with
   controller-text
   reg-bindings
   default-ops-builder))

;; create an operation list builder
;; which does not need to retrieve data from machine
;; instances
(define pure-builder
  const)

;; union builders together, leftmost matching entity is always preferred
;; the union is done at the creation of the actual operation list
;; note that for simplicity shadowed operations are not removed
(define (ops-builder-union . builders)
  (lambda (m)
    (apply append
           (map (lambda (x)
                  (x m))
                builders))))

;; NOTE: consider using it with functions like "build-and-execute-with"
;; instead of overwriting it and just runing the "build-and-execute"
;; I know I've done it wrongly for a long time.
;; TODO: there should only be one universal "default-ops-builder"
;; fix this in all ch05 exercises!
;; TODO: checklist:
;; * exercises: 23,24,42
;; * patches: compiler,ec
(define default-ops-builder
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
       (odd? ,odd?)
       (even? ,even?)
       (print ,out)
       (not ,not)

       ;; I guesss I still need to provide "initialize-stack".
       ;; this primitive is often used when re-initializing the machine
       ;; is more expensive
       (initialize-stack
        ,(lambda ()
           (machine-set-stack! m (empty-stack))))

       ;; "debug" allows us to run arbitrary procedure
       ;; taking either register values or constants as arguments
       ;; example:
       ;; * (perform (op debug) (const ,(lambda (x) (+ x 10))) (reg a))
       ;; * (assign a (op debug) (const ,(lambda (x y) (+ x y)))
       ;;                        (reg x) (reg y))
       ;; be sure to use quasiquotes if you want this to work
       (debug ,(lambda (proc . args)
                 (apply proc args)))
       )))
