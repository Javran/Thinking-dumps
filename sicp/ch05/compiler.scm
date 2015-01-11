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
