;; see notes in "5_3_2_maintaining_the_illusion_of_infinite_memory.md"
;; for related discussions

(load "./rewrite-instructions.scm")
(load "./list-stack-rewrites.scm")

(define (constant-exp-value exp)
  (define (valid-constant? data)
    ;; now data can only be one of:
    ;; symbol, number, boolean, string, char or null
    (or (symbol? data)
        (number? data)
        (boolean? data)
        (string? data)
        (char? data)
        (null? data)))
  (let ((data (cadr exp)))
    (if (valid-constant? data)
        data
        (error "cannot use" data
               "as a constant"))))

;; registers that should always be presented
(define reserved-registers
  '(pc flag the-cars the-cdrs the-stack))

(define (make-machine register-names
                      ops
                      controller-text)
  (let ((machine (make-new-machine)))
    (for-each
     (lambda (register-name)
       ((machine 'allocate-register) register-name))
     ;; remvoe "pc" and "flag" when
     ;; we are creeating the machine,
     ;; because these two registers have been created beforehand
     (remove-duplicates
      (append
       ;; registers required by this lower expansion
       (set-diff (append register-names reserved-registers)
                 '(pc flag)))))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define memory-size 65536)

(define (pointer n)
  (cons 'ptr n))

;; check if the data is a machine pointer
(define (pointer? data)
  (and (pair? data)
       (eq? (car data) 'ptr)
       (integer? (cdr data))))

(define (pointer-get data)
  (assert (pointer? data)
          "can only extract data from pointers")
  (cdr data))

;; next "memory location"
(define (pointer-inc data)
  (pointer
   (add1
    (pointer-get data))))

;; pointer equality
;; enforcing proper pointers being passed to it
(define (pointer=? p1 p2)
  (assert (pointer? p1)
          "expecting a pointer")
  (assert (pointer? p2)
          "expecting a pointer")
  (equal? p1 p2))

(define default-primitive-list
  (let ((old-primitive-list default-primitive-list))
    (lambda ()
      `((vector-ref
         ,(lambda (vec ptr)
            (vector-ref vec (pointer-get ptr))))
        (vector-set!
         ,(lambda (vec ptr val)
            (vector-set! vec (pointer-get ptr) val)))
        (to-ptr ,pointer)
        (ptr-inc ,pointer-inc)
        (pair? ,pointer?)
        (ptr=? ,pointer=?)
        (null? ,null?)
        (number? ,number?)
        (symbol? ,symbol?)
        (char? ,char?)
        (string? ,string?)
        ,@(old-primitive-list)))))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           ;; the primitive "initialize-stack"
           ;; has been replaced with a lower level rewrite,
           ;; no need to keep it.
           '())
          (register-table
           (list (list 'pc pc)
                 (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register:"
                   name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:"
                     name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               ;; initialize some reserved registers here
               ;; when the machine starts
               (set-contents!
                (lookup-register 'free)
                (pointer 0))
               (set-contents!
                (lookup-register 'the-cars)
                (make-vector memory-size))
               (set-contents!
                (lookup-register 'the-cdrs)
                (make-vector memory-size))
               (set-contents!
                (lookup-register 'the-stack)
                '())
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register)
               allocate-register)
              ((eq? message 'get-register)
               lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else
               (error "Unknown request: MACHINE"
                      message))))
      dispatch)))

(define (tranform-instructions insns)
  (rewrite-instructions* all-rules insns))

;; remove "save" and "restore" because they are no longer needed
(define make-save #f)
(define make-restore #f)

(define (make-execution-procedure
         inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else
         (error "Unknown instruction type: ASSEMBLE"
                inst))))
