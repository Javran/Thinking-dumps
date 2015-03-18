(load "exercise_5_43_common.scm")
(load "exercise_5_43_compiler.scm")

;; open-code support from exercise 5.38
(load "exercise_5_38_transform.scm")

(load "exercise_5_44_open-code.scm")

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
    (compile-application-maybe-open-code exp target linkage ctenv))
   (else
    (error "Unknown expression type: COMPILE" exp))))

;; compile function applications, use open-code compilation
;; whenever correctness can be preserved
(define (compile-application-maybe-open-code
         exp
         target
         linkage
         ctenv)
  (let* ((rator (car exp))
         (rands (cdr exp))
         (ct-result (find-variable rator ctenv)))
    (if (equal? ct-result 'not-found)
        ;; if compile-time environment fails to find a binding,
        ;; then open-code compilation could be applied here
        (let ((arg-length (length rands)))
          (cond
           ((= arg-length 2)
            (cond
             ((open-code-bin-op-=? exp)
              (compile-open-code-bin-op-= exp target linkage ctenv))
             ((open-code-bin-op-*? exp)
              (compile-open-code-bin-op-* exp target linkage ctenv))
             ((open-code-bin-op--? exp)
              (compile-open-code-bin-op-- exp target linkage ctenv))
             ((open-code-bin-op-+? exp)
              (compile-open-code-bin-op-+ exp target linkage ctenv))
             ;; be aware that we still need a fallback here
             ;; in case the 2-argument function application
             ;; cannot be compiled using the handlers above
             (else
              (compile-application exp target linkage ctenv))))
           ((assoc rator transformable-table) =>
            (lambda (p)
              (compile (transform-right exp (cadr p)) target linkage ctenv)))
           (else
            (compile-application exp target linkage ctenv))))
        ;; otherwise the variable is bound to something else
        ;; we'd better keep it as it is
        (compile-application exp target linkage ctenv))))
