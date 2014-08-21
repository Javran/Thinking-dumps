;; Let's try to create a better machine simulator
;; and name it "simu"

(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; toggle tests
(define *simu-test* #t)

(load "./simu_utils.scm")
(load "./simu_handlers.scm")
(load "./simu_accessors.scm")
(load "./simu_machine.scm")

(define (assemble controller-text machine)
  ;; expected input: a list of instruction texts (including labels)
  ;; we don't have to do all the things in one pass
  ;; as this won't be the bottleneck of the whole program
  ;; we also don't need to perform the continuation passing trick
  ;;
  ;; to be clear with terms:
  ;; instruction text is what the instruction looks like in controller text
  ;; (e.g. (assign x (const 1)))
  ;; instruction execution procedure is a procedure without arguments.
  ;; it performs the operation described by the instruction text when executed.
  ;; instruction is a pair consisted of instruction text and instruction-exec-proc
  ;;
  ;; in the first pass, we simply turn instruction text into
  ;; a pair: (<instruction-text> . <instruction-execution-procedure>)

  ;; and in the second pass, we make the label-instruction alist
  (define (make-instruction insn-text)
    (if (symbol? insn-text)
        ;; labels are kept as it is when making instructions
        insn-text
        ;; deal with real instructions
        (cons insn-text
              (make-execution-procedure insn-text machine))))
  (let ((insns (map make-instruction controller-text)))
    ;; TODO
    (for-each out insns))
  )

(define (make-execution-procedure insn-text machine)
  ;; TODO: make alists instead of passing these arguments
  ;; which is error prone.
  ;; since we only run this once for each instruction,
  ;; I think the performance won't be an issue
  (let ((handler (get-handler (car insn-text))))
    (if handler
        (let ((jump-table (machine-jump-table machine))
              (pc (machine-find-register machine 'pc))
              (flag (machine-find-register machine 'flag))
              (stack (machine-stack machine))
              (ops (machine-operations machine)))
          ;; TODO: rearrange arguments
          (handler insn-text jump-table machine pc flag stack ops))
        (error "unknown instruction:" inst))))

;; TODO: not confident if the current system will be working,
;; try to at least make some handlers work.
(let* ((inst '(assign a (op +) (reg b) (const 10)))
       (labels '*not-used*)
       (machine (make-machine
                 ;; register names
                 '(a b)
                 ;; operations (not sure how to represent
                 'todo
                 ;; controller text
                 'todo))
       (handler (get-handler (car inst))))
  'ok)


;; Local variables:
;; proc-entry: ""
;; End:
