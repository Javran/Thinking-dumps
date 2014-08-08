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

