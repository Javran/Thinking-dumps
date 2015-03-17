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
    ;; an application is a non-empty list
    ;; therefore "car" and "cdr" are guaranteed to be safe
    (let ((arg-length (length (cdr exp)))
          (operator (car exp)))
      (cond
       ;; to avoid getting into an infinite loop,
       ;; the open-code compilers are only interested
       ;; in binary applications
       ;; therefore the syntactic transformation
       ;; is done only once, and the recursive compilation
       ;; on the transformed expression will lead up to here.
       ((= arg-length 2)
        (cond
         ;; TODO: need corresponding changes in open-code
         ;; because of the newly added compile-time environment
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
       ((assoc operator transformable-table) =>
        (lambda (p)
          (compile (transform-right exp (cadr p)) target linkage ctenv)))
       (else
        (compile-application exp target linkage ctenv)))))
   (else
    (error "Unknown expression type: COMPILE" exp))))
