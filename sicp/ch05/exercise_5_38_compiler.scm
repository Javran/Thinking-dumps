(load "./exercise_5_38_open-code.scm")
(load "./exercise_5_38_transform.scm")

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
         ((open-code-bin-op-=? exp)
          (compile-open-code-bin-op-= exp target linkage))
         ((open-code-bin-op-*? exp)
          (compile-open-code-bin-op-* exp target linkage))
         ((open-code-bin-op--? exp)
          (compile-open-code-bin-op-- exp target linkage))
         ((open-code-bin-op-+? exp)
          (compile-open-code-bin-op-+ exp target linkage))
         ;; be aware that we still need a fallback here
         ;; in case the 2-argument function application
         ;; cannot be compiled using the handlers above
         (else
          (compile-application exp target linkage))))
       ((assoc operator transformable-table) =>
        (lambda (p)
          (compile (transform-right exp (cadr p)) target linkage)))
       (else
        (compile-application exp target linkage)))))
   (else
    (error "Unknown expression type: COMPILE" exp))))

;; Local variables:
;; proc-entry: "./exercise_5_38.scm"
;; End:
