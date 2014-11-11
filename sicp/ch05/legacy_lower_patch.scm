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

(define reserved-registers
  ;; "pc" and "flag" registers are not that special
  ;; it isn't a very good idea to make them special in the original design
  ;; because that design makes it complicated
  ;; when you want to reserve more registers
  '(pc flag the-cars the-cdrs the-stack))

(define (extract-register-names instructions)
  (define (extract insn)
    (if (symbol? insn)
        '()
        (let ((names1
               (cond
                ((or (tagged-list? insn 'assign)
                     (tagged-list? insn 'save)
                     (tagged-list? insn 'restore))
                 (list (cadr insn)))
                (else '())))
              (names2
               (map cadr
                    (filter (lambda (e)
                              (and (list? e)
                                   (eq? 'reg (car e))))
                            insn))))
          (append names1 names2))))
  (remove-duplicates
   (set-diff
    ;; make sure reserved registers exist
    (append
     (concat-map extract instructions)
     reserved-registers)
    ;; and delete "pc" and "flag"
    ;; because they are way more special
    ;; thanks to the original design
    '(pc flag))))

(define memory-size 65536)

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
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

               ;; TODO initialize free
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
