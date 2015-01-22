;; this module includes code generators that
;; generate no branches by themselves

;; compile self-evaluating expresssions
(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    ;; requires no register
    ;; assigns value to the target register
    '() (list target)
    `((assign ,target (const ,exp))))))

;; compile quoted expressions
(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    ;; almost the same as "self-evaluating"
    '() (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))))

;; compile variable expressions
(define (compile-variable exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    ;; requires "env" register for environment looking up
    ;; assigns value to the target register
    '(env)
    (list target)
    `((assign ,target
              (op lookup-variable-value)
              (const ,exp)
              (reg env))))))

;; compile assignments
(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        ;; an instruction sequence for evaluating
        ;; the value
        (get-value-code
         (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage
     (preserving
      '(env)
      ;; insert code that evaluates the value
      get-value-code
      ;; and proceed (compile with linkage=next)
      ;; to assign the value to a target
      (make-instruction-sequence
       '(env val) (list target)
       `((perform (op set-variable-value!)
                  (const ,var)
                  (reg val)
                  (reg env))
         ;; an assignment returns "ok" symbol
         (assign ,target (const ok))))))))

;; compile definitions
(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        ;; almost like assignment expresssions
        (get-value-code
         (compile (definition-value exp) 'val 'next)))
    (end-with-linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val) (list target)
       '((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         ;; a definition returnes "ok" symbol
         (assign ,target (const ok))))))))

;; compile a sequence of expressions (used by "begin" form)
(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      ;; avoiding redundant preservings
      (compile (first-exp seq) target linkage)
      (preserving
       ;; when we end an instruction sequence with linkage,
       ;; it might use "continue" register to jump back
       ;; in which case we need to preserve "continue" register
       '(env continue)
       (compile (first-exp seq) target 'next)
       (compile-sequence (rest-exps seq) target linkage))))
