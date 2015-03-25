;; compiler patch for ex 5.36
(set! primitive-operations
      (set-union
       '(snoc)
       primitive-operations))

;; appends one element to the end of a list
(define (snoc xs v)
  (append xs (list v)))

(define (compile-application exp target linkage)
  (define (construct-arglist-left-to-right operand-codes)
    (fold-left (lambda (acc cur-operand)
                 ;; map fusion here.
                 ;; * (fold-left g seed (map f xs))
                 ;; is rewritten as:
                 ;; * (fold-left (lambda (a i)
                 ;;                (g a (f i))) seed xs)
                 ;; avoiding redundant traversals.
                 (preserving
                  ;; the environment should be preserved
                  ;; between evaluation of operands
                  '(env)
                  acc
                  (preserving
                   ;; the argument list should be preserved
                   ;; so that the one we are building won't get
                   ;; overwritten.
                   '(argl)
                   cur-operand
                   (make-instruction-sequence
                    '(val argl) '(argl)
                    '((assign argl (op snoc) (reg argl) (reg val)))))))
               (make-instruction-sequence
                '() '(argl)
                '((assign argl (const ()))))
               operand-codes))
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
         (map
          (lambda (operand)
            (compile operand 'val 'next))
          (operands exp))))
    (preserving
     '(env continue)
     ;; evaluate operator
     proc-code
     (preserving
      '(proc continue)
      ;; evaluate operands, from left to right
      ;; and construct the argument list
      (construct-arglist-left-to-right operand-codes)
      ;; dispatch to either a primitive procedure or composite one
      (compile-procedure-call target linkage)))))

;; Local variables:
;; proc-entry: "./exercise_5_36.scm"
;; End:
