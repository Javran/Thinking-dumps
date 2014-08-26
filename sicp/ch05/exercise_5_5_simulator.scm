;; accepts a list of instructions
(define (make-jump-alist instructions)
  (let loop ((insns instructions)
             (jump-alist '()))
    (if (null? insns)
        jump-alist
        (if (symbol? (car insns))
            ;; add a new label
            (loop
             (cdr insns)
             (cons (list (car insns) (cdr insns))
                   jump-alist))
            ;; skip it
            (loop
             (cdr insns)
             jump-alist)))))

(define (make-machine-with-insns insns)
  (let ((ms (make-empty-machine)))
    (ms-jump-alist-set
     (make-jump-alist insns)
     (ms-insns-set insns ms))))

(define (simulate-insn insn ms)
  (cond ((symbol? insn)
         ;; skip labels
         ms)
        ((and (non-empty? insn)
              (get-handler (car insn)))
         => (lambda (handler)
              (handler (cdr insn) ms)))
        (else
         (error "handle not found for"
                insn))))

(define (run-machine ms)
  (let ((insns (ms-insns ms)))
    (if (null? insns)
        ;; simulation is done
        ms
        ;; simulate the next insn
        (let ((insn (car insns)))
          (ms-pretty-print ms)
          (run-machine
           (simulate-insn
            insn
            (ms-insns-inc ms)))))))

(define (execute-controller-with-regs controller regs)
  (run-machine
   (ms-set-field
    'regs regs
    (make-machine-with-insns
     (cdr controller)))))
