(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")

(define (save-handler insn m)
  (let* ((reg-name (stack-insn-reg-name insn))
         (reg (machine-find-register m reg-name))
         (stack (machine-stack m)))
    (lambda ()
      (stack-push! stack
                   (cons reg-name (register-get reg)))
      (advance-pc m))))
(set-handler 'save save-handler)

(define (restore-handler insn m)
  (let* ((reg-name (stack-insn-reg-name insn))
         (reg (machine-find-register m reg-name))
         (stack (machine-stack m)))
    (lambda ()
      (let* ((top-element (stack-top stack))
             (top-reg-name (car top-element))
             (top-reg-value (cdr top-element)))
        (if (eq? top-reg-name reg-name)
            (begin
              (register-set! reg top-reg-value)
              (stack-pop! stack)
              (advance-pc m))
            (error (format
                    #f
                    "register name '~A' expected, but '~A' found"
                    top-reg-name reg-name)))))))
(set-handler 'restore restore-handler)

(load "./exercise_5_11_b_test_controllers.scm")

(let ((m (build-and-execute controller-fine '())))
  (out (machine-reg-get m 'a))
  ;; 1
  (out (machine-reg-get m 'b))
  ;; 2
  'ok)

(assert-error
 (lambda ()
   (let ((m (build-and-execute controller-fail '())))
  (out (machine-reg-get m 'a))
  (out (machine-reg-get m 'b))))
 "restoring from a different register causes an error")
