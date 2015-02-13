(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./compiler.scm")

(define all-regs
  '(env proc val argl continue arg1 arg2))

;; spread-arguments takes a list of operands
;; and assign each of them into the corresponding
;; target registers, note that in order
;; to keep the order of operands' evaluation consistent
;; the operands are evaluated from right to left
;; e.g.
;;  (spread-arguments (list <exp1> <exp2>))
;; first evaluates "exp2", assigning the value to
;; "arg1" register, and then "exp1" is evaluated
;; and its value is assigned to "arg2"
;; * the length of the operand list must be less or equal to 2
;; * this function assumes the existence of register "arg1" and "arg2"
;;   which the target machine should provide
(define (spread-arguments operand-exps)
  ;; TODO: if we use "val" as "argument register",
  ;; can we handle "open-code" primitives that
  ;; takes 3 arguments?
  (let ((arg-list-len (length operand-exps))
        (compiled-operands
           (map (lambda (operand-exp target)
                  (compile operand-exp target 'next))
                operand-exps
                '(val val))))
    ;; TODO: I'm not sure if targeting registers other than "val" or "proc"
    ;; will yield problematic instruction lists. previously I recall there's
    ;; somewhere in the compiler that assume the target register being
    ;; either "val" or "proc".

    ;; for now, let's just go back to the exercise and think about "open-code"
    ;; primitives for the second time: an "(assign arg1 (reg val))" instruction
    ;; right after the evaluation will be fine, but it makes code more verbose
    ;; which violates what "open-code" primitive is doing.
    ;; TODO: arg-list < 2 is not yet covered
    (cond ((= arg-list-len 0) (empty-instruction-sequence))
          ((= arg-list-len 1) (car compiled-operands))
          ((= arg-list-len 2)
           ;; (out ">>> first insnseq")
           ;; (print-instruction-sequence (cadr compiled-operands))
           ;; (out ">>> second insnseq")
           ;; (print-instruction-sequence (car  compiled-operands))
           ;; (out "<<<")
           ;; TODO: not working. this "spread-arguments" is a design failure
           ;; because it has no way to aware of interactions between
           ;; argument evaluations, for example the previous expression
           ;; might modify a register and the second one can overwrite
           ;; it whlie preseving" does nothing.
           ;; just as suggested by http://community.schemewiki.org/?sicp-ex-5.38
           ;; this function should return a list instead of a single instruction seq

           ;; I prefer crash over wrong code
           (error "not working")
           (preserving
            ;; note that the operands should be evaluated from right to left
            '(arg2)
            (append-instruction-sequences
             (cadr compiled-operands)
             (make-instruction-sequence
              '(val) '(arg2)
              '( (assign arg2 (reg val)) )))
            (append-instruction-sequences
             (car compiled-operands)
             (make-instruction-sequence
              '(val) '(arg1)
              '( (assign arg1 (reg val)) )))))
          (else
           (error "the length of the operand list must not exceed 2")))))

(define (generate-open-code-compiler-for-binary bin-op)
  (lambda (exp target linkage)
    (let ((rator (car exp))
          (rands (cdr exp)))
      (end-with-linkage
       linkage
       (append-instruction-sequences
        (spread-arguments rands)
        (make-instruction-sequence
         '(arg1 arg2)
         (list target)
         `( (assign ,target (op ,bin-op) (reg arg1) (reg arg2)) )))))))

(define (generate-open-code-predicate-for-binary bin-op)
  (lambda (exp)
    (and ((list-tagged-with bin-op) exp)
         (= (length exp) 3))))

(define compile-open-code-bin-op-=
  (generate-open-code-compiler-for-binary '=))

(define open-code-bin-op-=?
  (generate-open-code-predicate-for-binary '=))

(define (compile exp target linkage)
  (cond
   ((self-evaluating? exp)
    (compile-self-evaluating exp target linkage))
   ((quoted? exp)
    (compile-quoted exp target linkage))
   ((variable? exp)
    (compile-variable exp target linkage))
   ((assignment? exp)
    (compile-assignment exp target linkage))
   ;; desugar before compiling
   ((definition? exp)
    (compile-definition
     (normalize-define exp)
     target linkage))
   ((if? exp)
    (compile-if exp target linkage))
   ((lambda? exp)
    (compile-lambda exp target linkage))
   ((begin? exp)
    (compile-sequence
     (begin-actions exp) target linkage))
   ((cond? exp)
    (compile (cond->if exp) target linkage))
   ;; adding let-form
   ((let? exp)
    (compile (let->combination exp) target linkage))
   ((application? exp)
    (cond
     ((open-code-bin-op-=? exp)
      (compile-open-code-bin-op-= exp target linkage))
     (else
      (compile-application exp target linkage))))
   (else
    (error "Unknown expression type: COMPILE" exp))))

(print-instruction-sequence
 (compile '(= (= x y) 2) 'val 'next))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
