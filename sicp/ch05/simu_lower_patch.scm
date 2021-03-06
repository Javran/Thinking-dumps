;; see notes in "5_3_2_maintaining_the_illusion_of_infinite_memory.md"
;; for related discussions

;; the real implementation
;; of list and stack primitives

(load "rewrite-instructions.scm")
(load "list-stack-rewrites.scm")

;; in the original simu.scm,
;; we can store anything we like in a register
;; and don't pay much attention when it comes to
;; pairs and storage.
;; this patch puts the machine to a "lower" level:
;; we now only keep value of basic types in a register
;; and on the stack, pairs will be represented in memory
;; and as "pointers".

(define (make-primitive-exp exp m)
  (define (constant-exp? exp)
    (tagged-list? exp 'const))
  (define (valid-constant? data)
    ;; now data can only be one of:
    ;; symbol, number, boolean, string, char or null
    (or (symbol? data)
        (number? data)
        (boolean? data)
        (string? data)
        (char? data)
        (null? data)))
  (define (constant-exp-value exp)
    (let ((data (cadr exp)))
      (if (valid-constant? data)
          data
          (error "cannot use" data
                 "as a constant"))))

  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (lambda ()
           (machine-lookup-label
            m (label-exp-label exp))))
        ((register-exp? exp)
         (let ((r (machine-find-register
                   m (register-exp-reg exp))))
           (lambda () (register-get r))))
        (else
         (error "unexpected expression:" exp))))

;; when defining registers, a list of reserved register
;; names are also defined.
(define (machine-define-registers! m regs-all)
  (define regs
    (remove-duplicates
     `(,@machine-reserved-registers ,@regs-all)))
  (machine-set-register-table!
   m
   (map (lambda (name)
          (list name (new-register)))
        regs)))

;; a list of registers that must be
;; present in a machine
(define machine-reserved-registers
  '(pc flag the-cars the-cdrs the-stack))

;; make a machine pointer
;; that represents a "memory location"
(define (machine-pointer n)
  (cons 'ptr n))

;; check if the data is a machine pointer
(define (machine-pointer? data)
  (and (pair? data)
       (eq? (car data) 'ptr)
       (integer? (cdr data))))

(define (machine-pointer-get data)
  (assert (machine-pointer? data)
          "can only extract data from pointers")
  (cdr data))

;; machine pointer equality
;; enforcing proper pointers being passed to it
(define (machine-pointer=? p1 p2)
  (assert (machine-pointer? p1)
          "expecting a pointer")
  (assert (machine-pointer? p2)
          "expecting a pointer")
  (equal? p1 p2))

;; next "memory location"
(define (machine-pointer-inc data)
  (machine-pointer
   (add1
    (machine-pointer-get data))))

(define (lower-ops-builder-extra m)
  ;; all primitives will be applied directly
  ;; using the value stored either in registers
  ;; or in a constant expression.
  ;; since almost everything except for pairs
  ;; are allowed in this new representation,
  ;; all of the existing primitives works here
  ;; but for things like `vector-ref` and `vector-set!`
  ;; we need something special,
  ;; `the-cars` and `the-cdrs` are two very special registers
  ;; since they are storing vectors rather than regular data
  ;; and these two primitives deal with machine-pointers,
  ;; which we need to extract the actual "addresses"
  ;; (i.e. the integers stored) from.
  `((vector-ref
     ,(lambda (vec ptr)
        (vector-ref vec (machine-pointer-get ptr))))
    (vector-set!
     ,(lambda (vec ptr val)
        (vector-set! vec (machine-pointer-get ptr) val)))
    ;; convert an integer into a pointer
    (to-ptr ,machine-pointer)
    ;; increase a pointer
    (ptr-inc ,machine-pointer-inc)
    ;; to test if the data under machine representation
    ;; is a pair is to test if the data is actually a machine-pointer
    (pair? ,machine-pointer?)
    (ptr=? ,machine-pointer=?)
    ;; some predicates can be lift costless.
    (null? ,null?)
    (number? ,number?)
    (symbol? ,symbol?)
    (char? ,char?)
    (string? ,string?)
    ;; "debug-print" prints operands separated by spaces
    (debug-print ,(lambda args
                    (for-each
                     (lambda (x)
                       (display x) (display " "))
                     args)
                    (newline)))))

(define (build-and-execute controller-text reg-bindings)
  (build-and-execute-with
   controller-text
   reg-bindings
   (ops-builder-union
    lower-ops-builder-extra
    default-ops-builder)))

(define machine-memory-size 65536)

(define (machine-fresh-start! m)
  ;; initialize two pieces of memories
  (machine-reg-set! m 'free (machine-pointer 0))
  (machine-reg-set! m 'the-cars (make-vector machine-memory-size))
  (machine-reg-set! m 'the-cdrs (make-vector machine-memory-size))
  (machine-reg-set! m 'the-stack '())
  (machine-reset-pc! m)
  (machine-execute! m))

(define (machine-do-insn-list-preprocess insns)
  (rewrite-instructions* all-rules insns))

;; remove "save" and "restore" handlers
;; since they are now implemented by other instructions
(set-handler 'save #f)
(set-handler 'restore #f)
