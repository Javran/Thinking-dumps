(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./legacy-easy.scm")

;; to care about the stack, we only need to
;; modify the part that manipulates the stack,
;; and only "save" and "restore" can do that.

(define (make-save inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    (lambda ()
      (push stack (cons reg-name (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
        (reg (get-register machine reg-name)))
    (lambda ()
      (let* ((top-element (pop stack))
             (top-reg-name (car top-element))
             (top-reg-value (cdr top-element)))
        (if (eq? top-reg-name reg-name)
            (begin
              (set-contents! reg top-reg-value)
              (advance-pc pc))
            (error (format
                    #f
                    "register name '~A' expected, but '~A' found"
                    top-reg-name reg-name)))))))

(let ((m (make-and-execute
          '(controller
            (assign a (const 1))
            (assign b (const 2))
            (save a)
            (save b)
            (assign a (const 10))
            (assign b (const 20))
            (restore b)
            (restore a))
          '())))
  (out (get-register-contents m 'a))
  (out (get-register-contents m 'b)))

(assert-error
 (lambda ()
   (let ((m (make-and-execute
          '(controller
            (assign a (const 1))
            (assign b (const 2))
            (save a)
            (save b)
            (assign a (const 10))
            (assign b (const 20))
            (restore a) ;; this part will raise an error
            (restore b))
          '())))
  (out (get-register-contents m 'a))
  (out (get-register-contents m 'b))))
 "restoring from a different register causes an error")
