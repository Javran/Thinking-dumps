(define (make-machine register-names
                      ops
                      controller-text)
  ;; register-names: a list of register names
  ;;   to be used in this machine model
  (let ((machine (make-new-machine)))
    (for-each
     (lambda (register-name)
       ((machine 'allocate-register) register-name))
     register-names)
    ((machine 'install-opeartions) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; registers
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond
       ((eq? message 'get) contents)
       ((eq? message 'set)
        (lambda (value)
          (set! contents value)))
       ;; seems like the "name" is not used anywhere...
       ;; let's make it useful.
       ((eq? message 'name) name)
       (else
        (error "unknown request: REGISTER"
               message))))
    dispatch))

(define (get-contents register)
  (register 'get))
(define (set-contents! register value)
  ((register 'set) value))
(define (get-reg-name register)
  (register 'name))

;; stack
(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define pop
      (let ((top (if (null? s)
                     (error "empty stack: POP")
                     (car s))))
        (set! s (cdr s))
        top))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) pop)
            ((eq? message 'initialize) (initialize))
            (else
             (error "Unknown request: STACK"
                    message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           ;; will this pollute the namespace?
           (list (list 'pc pc)
                 (list 'flag flag))))
      ;; allocate a new register
      ;; a unique name is enforced
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: "
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
              ((eq? message 'install-opeartions)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else
               (error "Unknown request: MACHINE"
                      message))))
      dispatch)))

(define (stack machine) (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name)
                 value)
  'done)
(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))
