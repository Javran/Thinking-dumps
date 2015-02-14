(load "./exercise_5_38_open-code.scm")

(set! primitive-operations
      (set-union primitive-operations
                 '(= * - +)))

(set! all-regs
      (set-union all-regs
                 '(arg1 arg2)))

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
   ((let? exp)
    (compile (let->combination exp) target linkage))
   ((application? exp)
    (cond
     ((open-code-bin-op-=? exp)
      (compile-open-code-bin-op-= exp target linkage))
     ((open-code-bin-op-*? exp)
      (compile-open-code-bin-op-* exp target linkage))
     ((open-code-bin-op--? exp)
      (compile-open-code-bin-op-- exp target linkage))
     ((open-code-bin-op-+? exp)
      (compile-open-code-bin-op-+ exp target linkage))
     (else
      (compile-application exp target linkage))))
   (else
    (error "Unknown expression type: COMPILE" exp))))
