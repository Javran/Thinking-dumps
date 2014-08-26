;; input a controller-text, and expected register values,
;; raise error if the actual register value is not equal to the expected value
(define (make-machine-test controller-text result-regs)
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

(make-machine-test
  '((assign a (op +) (const 20) (const 1))
    (test (op zero?) (const 1))
    (branch (label aa))
    (assign a (const 10))
    aa
    (assign a (op +) (reg a) (reg a)))
  '((a 20)))

;; Local variables:
;; proc-entry: "./simu.scm"
;; End:

