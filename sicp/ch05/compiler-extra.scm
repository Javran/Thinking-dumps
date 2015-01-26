;; some extra functionalities for the compiler

(define (print-instruction-sequence insn-seq)
  (format #t "Registers needed: ~A~%~
              Registers modified: ~A~%~
              ;;;; Instruction listing:~%"
          (registers-needed insn-seq)
          (registers-modified insn-seq))
  (for-each (lambda (insn)
              (format #t "  ~A~%" insn))
            (statements insn-seq))
  (out ";;;; End of listing"))

;; extract required operations from a list of instructions
;; operations are represented as (<operation-name> <arity>)
(define (extract-operations insns)
  (define (insn->operation insn)
    (if (pair? insn)
        (let ((head (car insn)))
          (cond ((and (eq? head 'assign)
                      (eq? (car (caddr insn)) 'op))
                 ;; (assign _ (op _) ..)
                 (list (list (cadr (caddr insn))
                             (- (length insn) 3))))
                ((or (eq? head 'test)
                     (eq? head 'perform))
                 ;; (test (op _) ..)
                 ;; (perform (op _) ..)
                 (list (list (cadr (cadr insn))
                             (- (length insn) 2))))
                (else
                 '())))
        '()))
  (remove-duplicates
   (concat-map insn->operation
               insns)))

;; compile the expression
;; and run it on the machine
(define (compile-and-run exp)
  (let* ((compiled (compile exp 'val 'next))
         (insn-seq (statements compiled)))
    ;; TODO
    (for-each out (map car (extract-operations insn-seq)))
    ))
