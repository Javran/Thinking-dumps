(load "./exercise_5_15_legacy_insncounter_patch.scm")
(load "./exercise_5_16_legacy_tracing_patch.scm")

(define (make-instruction text prev-inst)
  (let ((prev-label (if (symbol? prev-inst)
                        prev-inst
                        #f)))
    (list text '() prev-label)))
(define instruction-text car)
(define instruction-execution-proc cadr)
;; mutability makes it smell
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst
            (list proc
                  (instruction-previous-label inst))))

(define instruction-previous-label caddr)

(define (extract-labels text prev-inst)
  (if (null? text)
      (cons '() '())
      (let ((result (extract-labels (cdr text) (car text))))
        (let ((insts (car result))
              (labels (cdr result)))
          (let ((next-inst (car text)))
            (if (symbol? next-inst)
                (cons insts
                      (cons (make-label-entry next-inst insts)
                            labels))
                (cons (cons (make-instruction
                             next-inst prev-inst) insts)
                      labels)))))))

(define (assemble controller-text machine)
  ;; an extra argument indicates the instruction
  ;; right before the current one processing
  (let ((result (extract-labels controller-text #f)))
    (let ((insts (car result))
          (labels (cdr result)))
      (update-insts! insts labels machine)
      insts)))

;; re-patch based on ex 5.16 patch
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-counter 0)
        (trace #f))
    (let ((the-ops
           (list
            (list 'initialize-stack
                  (lambda ()
                    (stack 'initialize)))
            (list 'trace-on
                  (lambda ()
                    (set! trace #t)))
            (list 'trace-off
                  (lambda ()
                    (set! trace #f)))
            ;; seems we have to modify the primitive list here
            (list 'print-insn-counter
                  (lambda ()
                    (format #t "# instruction executed: ~A~%"
                            instruction-counter)))
            (list 'reset-insn-counter
                  (lambda ()
                    (set! instruction-counter 0)))
            ))
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
        ;; re-patch this part so it can print out previous labels
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (let* ((inst (car insts))
                     (proc (instruction-execution-proc inst))
                     (text (instruction-text inst))
                     (lbl  (instruction-previous-label inst)))
                (if trace
                    (begin
                      (if lbl
                          (format #t "into label: ~A~%" lbl)
                          'skipped)
                      (out text))
                    'skipped)
                (proc)
                ;; bump counter
                (set! instruction-counter
                      (add1 instruction-counter))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               ;; initialize instruction counter
               (set! instruction-counter 0)
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
              ((eq? message 'trace?) trace)
              ((eq? message 'trace-on)
               (set! trace #t))
              ((eq? message 'trace-off)
               (set! trace #f))
              ;; new messages: get and reset counter
              ((eq? message 'get-insn-counter) instruction-counter)
              ((eq? message 'reset-insn-counter)
               (set! instruction-counter 0))
              (else
               (error "Unknown request: MACHINE"
                      message))))
      dispatch)))

;; Local variables:
;; proc-entry: "./exercise_5_17_legacy.scm"
;; End:
