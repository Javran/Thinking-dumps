(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "legacy-easy.scm")

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

(load "exercise_5_11_b_test_controllers.scm")

(let ((m (make-and-execute controller-fine '())))
  (out (get-register-contents m 'a))
  ;; 1
  (out (get-register-contents m 'b))
  ;; 2
  'ok)

(assert-error
 (lambda ()
   (let ((m (make-and-execute controller-fail '())))
  (out (get-register-contents m 'a))
  (out (get-register-contents m 'b))))
 "restoring from a different register causes an error")
