(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./legacy-easy.scm")

(define (make-execution-procedure
         inst labels machine pc flag stack ops)
  (cond
   ;; instruction `assign` is removed
   ;; new syntax: (copy <reg> <src>)
   ((eq? (car inst) 'copy)
    (make-copy inst machine labels ops pc))
   ;; new syntax: (call <reg> <op> <arg1> <arg2> ...)
   ((eq? (car inst) 'call)
    (make-call inst machine labels ops pc))
   ((eq? (car inst) 'test)
    (make-test inst machine labels ops flag pc))
   ((eq? (car inst) 'branch)
    (make-branch inst machine labels flag pc))
   ((eq? (car inst) 'goto)
    (make-goto inst machine labels pc))
   ((eq? (car inst) 'save)
    (make-save inst machine stack pc))
   ((eq? (car inst) 'restore)
    (make-restore inst machine stack pc))
   ((eq? (car inst) 'perform)
    (make-perform inst machine labels ops pc))
   (else
    (error "Unknown instruction type: ASSEMBLE"
           inst))))

(define (make-copy inst machine labels operations pc)
  (let* ((target
          ;; target register name
          (cadr inst))
         (source
          ;; source: one of label/reg/const
          (caddr inst))
         (value-proc
          (make-primitive-exp
           source machine labels)))
    (lambda ()
      (set-contents! target (value-proc))
      (advance-pc pc))))

(end-script)
