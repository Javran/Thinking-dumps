;; TODO:
;; yet again I don't know what I'm doing,
;; but this will eventually turn out to be useful...

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
   ((definition? exp)
    (compile-definition exp target linkage))
   ((if? exp)
    (compile-if exp target linkage))
   ((lambda? exp)
    (compile-lambda exp target linkage))
   ((begin? exp)
    (compile-sequence
     (begin-actions exp) target linkage))
   ((cond? exp)
    (compile (cond->if exp) target linkage))
   ((application? exp)
    (compile-application exp target linkage))
   (else
    (error "Unknown expression type: COMPILE" exp))))

;; the instruction sequence is also keeping some extra
;; information to avoid doing redundant analyzing work
;; need: the set of registers must be initialized before
;;   execution
;; modifies: the set of registers whose value are modified
;;   by the instruction sequence
;; statements: the instruction sequence
(define (make-instruction-sequence
         needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

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
          ;; note: backquote here.
          `((goto (label ,linkage)))))))

;; a sequence of instructions will be finalized by
;; some instrcutions taking care of the linkage
;; possible linkages:
;; - return: jump back using "continue" register
;; - next: do nothing, just continue execution
;; - <otherwise>: jump to a label specified by "linkage" argument
(define (end-with-linkage linkage instruction-sequence)
  (preserving
   '(continue)
   instruction-sequence
   (compile-linkage linkage)))

;; simple expressions
(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '() (list target)
    `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '() (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '(env) ;; requires "env" register
    (list target)
    `((assign ,target
              (op lookup-variable-value)
              (const ,exp)
              (reg env))))))
