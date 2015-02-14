(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./compiler.scm")
(load "./simu.scm")
(load "./simu_compiler_patch.scm")

(define all-regs
  '(env proc val argl continue arg1 arg2))

;; by searching "(op <something>)" from compiler's source code
;; we are able to extract all possible primitives that
;; our compiled code might use.
(define primitive-operations
  '(false?
    lookup-variable-value
    set-variable-value!
    define-variable!
    make-compiled-procedure
    compiled-procedure-env
    extend-environment
    list
    cons
    compiled-procedure-entry
    primitive-procedure?
    apply-primitive-procedure
    =
    *
    -
    +))

;; spread-arguments takes a list of operands
;; and assign each of them into the corresponding
;; target registers. a list of compiled instruction sequences
;; is returned. you need to append them together and do register preserving.
;; * the length of the operand list must be less or equal to 2
;; * this function assumes the existence of register "arg1" and "arg2"
;;   which the target machine should provide
(define (spread-arguments operand-exps)
  (let ((arg-list-len (length operand-exps)))
    (assert (<= arg-list-len 2))
    ;; TODO: I'm not sure if targeting registers other than "val" or "proc"
    ;; will yield problematic instruction lists. previously I recall there's
    ;; somewhere in the compiler that assume the target register being
    ;; either "val" or "proc".

    ;; TODO: arg-list < 2 is not yet covered
    (map (lambda (operand-exp target)
           (compile operand-exp target 'next))
         operand-exps
         '(arg1 arg2))))

;; open code compiler generator for binary functions
;; takes a binary operation symbol, and generates a compiler
;; procedure that does open-code compiling for that symbol
;; note that in order to keep the order of
;; operands' evaluation consistent,
;; the operands are evaluated from right to left
;; e.g. to compile arguments for the expression:
;;   (= <exp1> <exp2>)
;; first evaluates "exp2", assigning the value to
;; "arg1" register, and then "exp1" is evaluated
;; and its value is assigned to "arg2"
(define (generate-open-code-compiler-for-binary bin-op)
  (lambda (exp target linkage)
    (let* ((rator (car exp))
           (rands (cdr exp))
           (compiled-rands (spread-arguments rands)))
      ;; since the generator is only for binary functions
      (assert (= (length rands) 2))
      (end-with-linkage
       linkage
       (append-instruction-sequences
        ;; the second arg first
        (cadr compiled-rands)
        (preserving
         '(arg2)
         (car compiled-rands)
         (make-instruction-sequence
          '(arg1 arg2)
          (list target)
          `( (assign ,target (op ,bin-op) (reg arg1) (reg arg2)) ))))))))

(define (generate-open-code-predicate-for-binary bin-op)
  (lambda (exp)
    (and ((list-tagged-with bin-op) exp)
         (= (length exp) 3))))

;; it might be convenient to use the message dispatching framework
;; but as we only have 4 cases to take care, I don't think
;; it matters that much
(define compile-open-code-bin-op-=
  (generate-open-code-compiler-for-binary '=))
(define compile-open-code-bin-op-*
  (generate-open-code-compiler-for-binary '*))
(define compile-open-code-bin-op--
  (generate-open-code-compiler-for-binary '-))
(define compile-open-code-bin-op-+
  (generate-open-code-compiler-for-binary '+))

(define open-code-bin-op-=?
  (generate-open-code-predicate-for-binary '=))
(define open-code-bin-op-*?
  (generate-open-code-predicate-for-binary '*))
(define open-code-bin-op--?
  (generate-open-code-predicate-for-binary '-))
(define open-code-bin-op-+?
  (generate-open-code-predicate-for-binary '+))

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

(load "./ec-tests.scm")
(load "./exercise_5_23_tests.scm")
(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)
(newline)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
