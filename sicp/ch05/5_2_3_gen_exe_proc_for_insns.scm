;; TODO: unacceptable code
;; some serious redesign must be done after we get
;; the whole system to work
(define (make-execution-procedure
         inst labels machine pc flag stack ops)
  ;; NO! ... THIS PART IS EXTREMELY UGLY!
  ;; at least we can consider making arguments exactly the same
  ;; therefore dispatching will be enabled
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops pc))
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

;; assign
(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp
         (assign-value-exp inst)))
    (let ((value-proc
           (if (opeartion-exp? value-exp)
               (make-opeartion-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

;; test
(define (make-test inst machine labels opeartions flag pc)
  (let ((condition (test-condition inst)))
    (if (opeartion-exp? condition)
        (let ((condition-proc
               (make-opeartion-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc)))
        (error "Bad TEST instruction: ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))
