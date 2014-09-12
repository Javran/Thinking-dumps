(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./legacy-easy.scm")

;; I will make the stack a lazy assoc-list
;; because the design of legacy simulator
;; allows registers to be allocated at any time
;; so we shouldn't assume anything about registers

(define (make-stack-alist)
  (cons 'stack-alist '()))

(define (pop-stack-alist stack-alist reg-name)
  (let ((st (cdr (assoc reg-name (cdr stack-alist)))))
    (pop st)))

(define (push-stack-alist stack-alist reg-name val)
  (let ((p (assoc reg-name (cdr stack-alist))))
    (if p
        (let ((st (cdr p)))
          (push st val))
        ;; need to create one
        (let ((st (make-stack)))
          (set-cdr! stack-alist
                    (cons (cons reg-name st)
                          (cdr stack-alist)))
          (st 'initialize)
          (push st val)))))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        ;; modified: stack to stack-alist
        (stacka (make-stack-alist))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda ()
                         ;; modified: to initialize it
                         ;; we make a new object of stack-alist
                         (set! stacka (make-stack-alist))))))
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
              ((eq? message 'stack) stacka)
              ((eq? message 'operations) the-ops)
              (else
               (error "Unknown request: MACHINE"
                      message))))
      dispatch)))

;; now "push" and "pop" requires two argument
;; to specify the stack, one is "stack-alist" object,
;; another register name

(define (make-save inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    (lambda ()
      ;; push <stack-alist> <reg-name> <value>
      (push-stack-alist stack reg-name (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    (lambda ()
      ;; pop <stack-alist> <reg-name>
      (set-contents! reg (pop-stack-alist stack reg-name))
      (advance-pc pc))))

(define test-controller-success
  '(controller
    (assign a (const 10))
    (assign b (const 20))
    (save a)
    (save b)
    (assign a (const 40))
    (assign b (const 50))
    (restore a)
    (restore b)))

(define test-controller-failure
  '(controller
    (assign a (const 1))
    (save a)
    (assign a (const 2))
    (save a)
    (restore b)
    (restore b)))

(let ((m (make-and-execute
          test-controller-success
          '())))
  (out (get-register-contents m 'a))
  ;; 10
  (out (get-register-contents m 'b))
  ;; 20
  'done)

(assert-error
 (lambda ()
   (let ((m (make-and-execute
          test-controller-failure
          '())))
     (out (get-register-contents m 'b))))
 "registers are no longer sharing the same stack")

(end-script)
