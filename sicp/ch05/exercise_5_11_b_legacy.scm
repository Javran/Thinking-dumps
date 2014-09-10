(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./legacy-easy.scm")

;; to care about the stack, we only need to
;; modify the part that manipulates the stack,
;; and only "save" and "restore" can do that.

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
