(load "exercise_5_43_common.scm")
(load "exercise_5_43_compiler.scm")

;; open-code support from exercise 5.38
(load "exercise_5_38_open-code.scm")
(load "exercise_5_38_transform.scm")

;; open-code supports
(set! primitive-operations
      (set-union primitive-operations
                 '(= * - +)))

(set! all-regs
      (set-union all-regs
                 '(arg1 arg2)))

;; based on ex 5.40, including compile-time environment
(define (compile exp target linkage ctenv)
  (cond
   ((self-evaluating? exp)
    (compile-self-evaluating exp target linkage ctenv))
   ((quoted? exp)
    (compile-quoted exp target linkage ctenv))
   ((variable? exp)
    (compile-variable exp target linkage ctenv))
   ((assignment? exp)
    (compile-assignment exp target linkage ctenv))
   ((definition? exp)
    (compile-definition
     (normalize-define exp)
     target linkage ctenv))
   ((if? exp)
    (compile-if exp target linkage ctenv))
   ((lambda? exp)
    (compile-lambda exp target linkage ctenv))
   ((begin? exp)
    (compile-sequence
     (begin-actions exp) target linkage ctenv))
   ((cond? exp)
    (compile (cond->if exp) target linkage ctenv))
   ((let? exp)
    (compile (let->combination exp) target linkage ctenv))
   ((application? exp)
    (compile-application exp target linkage ctenv))
   (else
    (error "Unknown expression type: COMPILE" exp))))
