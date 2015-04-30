(load "compiler-label.scm")
(load "compiler-insn-seq.scm")

(load "compiler-no-branch.scm")
(load "compiler-if.scm")
(load "compiler-proc.scm")
(load "compiler-prim.scm")
(load "compiler-verify.scm")

(define all-regs
  '(env proc val argl continue))

;; TODO: support for "and" and "or"

;; exp: the expression to be compiled.
;; target: the target register to hold resulting value.
;; linkage: how should we proceed after the expression
;;   is evaluated.
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
   ;; adding and-form and or-form
   ((and? exp)
    (compile (and->if exp) target linkage))
   ((or? exp)
    (compile (or->if exp) target linkage))
   ((application? exp)
    (compile-application exp target linkage))
   (else
    (error "Unknown expression type: COMPILE" exp))))

;; a sequence of instructions will be finalized by
;; some instrcutions taking care of the linkage
;; possible linkages:
;; - return: jump back using "continue" register
;; - next: do nothing, just continue execution
;; - <otherwise>: jump to a label specified by "linkage" argument
;; NOTE: cannot use "next" and "continue" as labels
(define (end-with-linkage linkage instruction-sequence)
  (define (compile-linkage linkage)
    (cond ((eq? linkage 'return)
           (make-instruction-sequence
            '(continue) '()
            '((goto (reg continue)))))
          ((eq? linkage 'next)
           (empty-instruction-sequence))
          (else
           (make-instruction-sequence
            '() '()
            `((goto (label ,linkage)))))))
  (preserving
   '(continue)
   instruction-sequence
   (compile-linkage linkage)))
