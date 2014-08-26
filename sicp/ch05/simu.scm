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

  ;; despite that the original program does not consider this issue,
  ;; but it's entirely possible that two labels can point to the same location
  ;; which might result in some labels being accidentally included in
  ;; the instruction list. (e.g. instruction-text list:
  ;; (lbl1 lbl2 (assign foo (reg bar))) => ( (lbl1 (lbl2 (assign ...)))
  ;;                                         (lbl2 (assign ...)) )
  ;; while we really want: ( (lbl1 (assign ...))
  ;;                         (lbl2 (assign ...)) )
  ;; ).
  ;; here we drop all the labels to get the expected result
  (define (drop-labels insns)
    (filter (compose not symbol?) insns))

  (let ((insns (map make-instruction controller-text)))
    (let ((jump-table
           (let loop ((table '())
                      (insns insns))
             (if (null? insns)
                 table
                 (let ((hd (car insns))
                       (tl (cdr insns)))
                   (if (symbol? hd)
                       ;; label detected
                       (loop (cons (list hd (drop-labels tl))
                                   table)
                             tl)
                       (loop table
                             tl)))))))
      (machine-set-instruction-sequence! machine (drop-labels insns))
      (machine-set-jump-table! machine jump-table))))

(define (make-execution-procedure insn-text machine)
  (let ((handler (get-handler (car insn-text))))
    (if handler
        ;; we choose to keep arguments simple
        ;; as it is easier to understand and maintain.
        (handler insn-text machine)
        (error "unknown instruction:" inst))))

;; input a controller-text, and expected register values,
;; raise error if the actual register value is not equal to the expected value
(define (make-machine-test controller-text result-regs)
  (define (extract-register-names controller-text)
    (define (extract insn)
      (cond ((symbol? insn) '())
            ((and (non-empty? insn)
                  (eq? 'assign (car insn)))
             (list (cadr insn)))
            (else '())))
    (delq 'pc
          (delq 'flag
                (apply append (map extract controller-text)))))

  ;; result-regs: (list (list <reg-name> <reg-value>) ...)
  (let ((m (empty-machine))
        (reg-names (extract-register-names controller-text)))
    (machine-define-registers!
     m reg-names)
  (machine-set-operations!
   machine
   `( (+ ,+)
      (- ,-)
      (* ,*)
      (/ ,/)
      (zero? ,zero?)
      ))
  (assemble controller-text m)
  (machine-reset-pc! m)
  (machine-execute! m)

  (let ((testcases
         (map
          (lambda (result-reg-info)
            (mat m (car result-reg-info) (cadr result-reg-info)))
          result-regs)))
  (do-test
   machine-reg-get
   testcases))))

;; TODO: not confident if the current system will be working,
;; try to at least make some handlers work.
(let ((machine (empty-machine)))
  (machine-define-registers!
   machine
   '(a b c d))
  (machine-set-operations!
   machine
   `( (+ ,+)
      (- ,-)
      (* ,*)
      (/ ,/)
      (zero? ,zero?)
      ))
  (assemble '((assign a (op +) (const 20) (const 1))
              (test (op zero?) (const 1))
              (branch (label aa))
              (assign a (const 10))
              aa
              (assign a (op +) (reg a) (reg a)))

            machine)

  (machine-reset-pc! machine)
  (machine-execute! machine)
  (out (machine-reg-get machine 'a
                        )))

;; Local variables:
;; proc-entry: ""
;; End:
