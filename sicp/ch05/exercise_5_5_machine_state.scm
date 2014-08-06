;; a machine state is a list:
;; (list 'machine-state <alist>)
;;
;; where each element of <alist> is a list of:
;; (list <key> <value>)
;; at least containing the following fields:
;; `insns` is the current list of pending instructions
;; `jump-alist` is a list of label-instruction list pairs
;; `stack` is the current stack
;; `regs` is the current registers
;; `test-flag' is the result of previous compuation (default to #f)

;; test if the given object is a machine state object
;; (alist is not examined)
(define (machine-state? data)
  (and (list? data)
       (not (null? data))
       (eq? (car data) 'machine-state)))

;; raises an error if the object is invalid
;; otherwise nothing happens
(define (ensure-machine-state data)
  (if (machine-state? data)
      'ok
      (error "invalid machine state object")))

;; basic field accessors and mutators
(define (ms-get-field key data)
  (ensure-machine-state data)
  (let ((alist (cadr data)))
    (cadr (assoc key alist))))

(define (ms-set-field key value data)
  (ensure-machine-state data)
  (let ((alist (cadr data)))
    (list 'machine-state
          (cons (list key value)
                (del-assoc key
                           (cadr data))))))

(define (ms-modify-field key proc data)
  (let ((old-val (ms-get-field key data)))
    (ms-set-field key (proc old-val) data)))

;; field accessors
(define ms-insns
  ((curry2 ms-get-field) 'insns))
(define ms-jump-alist
  ((curry2 ms-get-field) 'jump-alist))
(define ms-stack
  ((curry2 ms-get-field) 'stack))
(define ms-regs
  ((curry2 ms-get-field) 'regs))

;; fetch a list of instructions using a label
;; `jump-alist` must contain this label
(define (ms-query-label lbl ms)
  (let ((jump-alist (ms-jump-alist ms)))
    (cadr (assoc lbl jump-alist))))

;; stack operations
(define ms-stack-top
  (compose car ms-stack))
(define (ms-stack-pop ms)
  (ms-modify-field 'stack cdr ms))
(define (ms-stack-push v ms)
  (ms-modify-field 'stack ((curry2 cons) v) ms))

;; register getter and setter
(define (ms-reg-set reg val ms)
  (ms-modify-field
   'regs
   (lambda (alist)
     (cons (list reg val)
           (del-assoc reg alist)))
   ms))
(define (ms-reg-get reg ms)
  (cadr (assoc reg (ms-regs ms))))

;; change the list of current instructions
(define (ms-insns-set insns ms)
  (ms-set-field 'insns insns ms))

;; test flag getter and setter
(define ms-test-flag
  ((curry2 ms-get-field) 'test-flag))
(define (ms-test-flag-set v ms)
  (ms-set-field 'test-flag v ms))

(define (make-empty-machine)
  `(machine-state
    ((insns ())
     (jump-alist ())
     (stack ())
     (regs ())
     (test-flag #f))))
