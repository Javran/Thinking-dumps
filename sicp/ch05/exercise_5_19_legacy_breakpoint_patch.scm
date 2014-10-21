(load "./exercise_5_17_legacy_prelabel_patch.scm")
(load "./exercise_5_19_breakpoint_table.scm")

;; TODO list:
;; * maintain resuming flag

;; based on ex 5.17 patch
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-counter 0)
        ;; maintain "after-label-counter"
        (after-label-counter 0)
        ;; maintain "resuming-flag"
        (resuming-flag #f)
        (trace #f)
        ;; maintain "current-label"
        (current-label #f)
        ;; a table containing all breakpoints
        (breakpoint-table '()))
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
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (let* ((inst (car insts))
                     (proc (instruction-execution-proc inst))
                     (text (instruction-text inst))
                     (lbl  (instruction-previous-label inst)))
                ;; update label if possible
                (if lbl
                    (begin
                      (set! current-label lbl)
                      (set! after-label-counter 0))
                    'skipped)
                ;; print out trace messages
                (if trace
                    (begin
                      (if lbl
                          (format #t "into label: ~A~%" lbl)
                          'skipped)
                      (out text))
                    'skipped)
                (if (and
                     (breakpoint-table-check?
                      current-label
                      (add1 after-label-counter)
                      breakpoint-table)
                     (not resuming-flag))
                    ;; need to break the execution here
                    (out "<breakpoint reached>")
                    ;; else keep going
                    (begin
                      (if resuming-flag
                          (set! resuming-flag #f)
                          'skipped)
                      (proc)
                      (set! instruction-counter
                            (add1 instruction-counter))
                      (set! after-label-counter
                            (add1 after-label-counter))
                      (execute)))))))
      ;; procedures requried by the exercise
      (define (set-breakpoint lbl n)
        (set! breakpoint-table
              (breakpoint-table-add
               lbl
               n
               breakpoint-table)))
      (define (proceed-machine)
        (set! resuming-flag #t)
        (execute))
      (define (cancel-breakpoint lbl n)
        (set! breakpoint-table
              (breakpoint-table-del
               lbl
               n
               breakpoint-table)))
      (define (cancel-all-breakpoints)
        (set! breakpoint-table '()))
      (define (dispatch message)
        (cond ((eq? message 'start)
               ;; initialize counters & flags
               (set! instruction-counter 0)
               (set! after-label-counter 0)
               (set! current-label #f)
               (set! resuming-flag #f)
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
              ((eq? message 'get-insn-counter) instruction-counter)
              ((eq? message 'reset-insn-counter)
               (set! instruction-counter 0))
              ;; expose procedures
              ((eq? message 'set-breakpoint)
               set-breakpoint)
              ((eq? message 'proceed-machine)
               proceed-machine)
              ((eq? message 'cancel-breakpoint)
               cancel-breakpoint)
              ((eq? message 'cancel-all-breakpoints)
               cancel-all-breakpoints)
              (else
               (error "Unknown request: MACHINE"
                      message))))
      dispatch)))

(define (set-breakpoint m lbl n)
  ((m 'set-breakpoint) lbl n))
(define (proceed-machine m)
  ((m 'proceed-machine)))
(define (cancel-breakpoint m lbl n)
  ((m 'cancel-breakpoint) lbl n))
(define (cancel-all-breakpoints m)
  ((m 'cancel-all-breakpoints)))

;; Local variables:
;; proc-entry: "./exercise_5_19_legacy.scm"
;; End:
