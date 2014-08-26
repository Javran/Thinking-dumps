;; input a controller-text, and expected register values,
;; raise error if the actual register value is not equal to the expected value
(define (do-machine-test controller-text result-regs)
  ;; since registers are unassigned at the begining,
  ;; to get a full list of registers we only need to take care
  ;; about "assign" instructions
  (define (extract-register-names controller-text)
    (define (extract insn)
      (cond ((symbol? insn) '())
            ((and (non-empty? insn)
                  (eq? 'assign (car insn)))
             (list (cadr insn)))
            (else '())))
    (remove-duplicates
     (set-diff
      (apply append (map extract controller-text))
      '(pc flag))))

  ;; result-regs: (list (list <reg-name> <reg-value>) ...)
  (let ((m (empty-machine))
        (reg-names (extract-register-names controller-text)))
    (machine-define-registers!
     m reg-names)
    (machine-set-operations!
     m
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

(define (do-handler-tests)
  ;; ==== test "assign" instruction ====
  ;; (const <val>)
  (do-machine-test
   '((assign a (const 1))
     (assign b (const 2))
     (assign b (const 3)))
   '((a 1)
     (b 3)))

  ;; (op <op>) and (reg <reg>)
  (do-machine-test
   '((assign a (const 1))
     (assign b (op +) (const 2) (reg a))
     (assign c (reg b)))
   '((a 1)
     (b 3)
     (c 3)))

  ;; this testcase is for test purpose only,
  ;; assigning to pc register is discouraged
  ;; because it might lead to confusions
  ;; (label <label>)
  (do-machine-test
   '((assign a (const 1))
     (assign pc (label jmp))
     (assign a (op +) (reg a) (const 10))
     jmp
     ;; the instruction right after the label
     ;; is skipped since "assign" always advances pc
     (assign a (op +) (reg a) (const 100))
     (assign a (op +) (reg a) (const 1000)))
   '((a 1001)))

  ;; ==== test "test" instruction ====
  ;; #f
  (do-machine-test
   '((test (op zero?) (const 1)))
   '((flag #f)))

  ;; #t, and with register access
  (do-machine-test
   '((assign a (const 0))
     (test (op zero?) (reg a)))
   '((flag #t)))

  ;; ==== test "branch" instruction ====
  ;; jump with #t flag
  (do-machine-test
   '((test (op zero?) (const 0))
     (assign a (const 1))
     (branch (label label-1))
     (assign a (op +) (reg a) (const 10))
     label-1
     (assign a (op +) (reg a) (const 100)))
   '((a 101)))

  ;; don't jump with #f flag
  (do-machine-test
   '((test (op zero?) (const 1))
     (assign a (const 1))
     (branch (label label-1))
     (assign a (op +) (reg a) (const 10))
     label-1
     (assign a (op +) (reg a) (const 100)))
   '((a 111)))

  'done)

(if *simu-test*
    (do-handler-tests)
    'ignored)

;; Local variables:
;; proc-entry: "./simu.scm"
;; End:

