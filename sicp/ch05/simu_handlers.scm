(load "./data-directed.scm")

;; ==== build handler lookup table

(define set-handler #f)
(define get-handler #f)
(define init-handler-table! #f)

(let* ((f-alist (global-table-functions))
       (set1 (cadr (assoc 'set f-alist)))
       (get1 (cadr (assoc 'get f-alist)))
       (init1 (cadr (assoc 'init f-alist))))
  (set! set-handler set1)
  (set! get-handler get1)
  (set! init-handler-table! init1))

;; ====
;; * (set-handler <slot> <handler>) to set a handler
;; * (get-handler <slot>) to retrieve a handler

;; analyze the list of instructions once for all,
;; yielding "thunks" which can be used multiple times
;; without too much time consumption.

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda ()
             c)))
        ((label-exp? exp)
         (let ((insts (lookup-label
                       labels
                       (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type: ASSEMBLE"
                exp))))

(define (register-exp? exp)
  (tagged-list? exp 'reg))
(define (register-exp-reg exp)
  (cadr exp))
(define (constant-exp? exp)
  (tagged-list? exp 'const))
(define (constant-exp-value exp)
  (cadr exp))
(define (label-exp? exp)
  (tagget-list? exp 'label))
(define (label-exp-label exp)
  (cadr exp))

;; handler type:
;; (<handler> inst labels machine pc flag stack ops)
(define (assign-handler inst labels machine pc flag stack ops)
  ;; accessors for "assign"
  ;; (assign <reg> @<value-exp> ..)
  (define assign-reg-name cadr)
  (define assign-value-exp cddr)

  (let ((target-reg
         (machine-find-register machine (assign-reg-name inst)))
        (value-exp
         (assign-value-exp inst)))
    (let ((value-proc
           ;; yields a value when run as a procedure
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()
        (register-set! target-reg (value-proc))
        ;; advance pc
        (machine-reg-set!
         machine 'pc
         (cdr (machine-reg-get machine 'pc))
         )))))
(set-handler 'assign assign-handler)

(define (test-handler inst labels machine pc flag stack ops)
  ;; instruction destruction
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            ;; execute the "operation", and then set the flag
            (set-contents! flag (condition-proc))
            (advance-pc)))
        ;; original error message is too verbose
        (error "TEST: bad instruction: "
               inst))))
(set-handler 'test test-handler)

(define (branch-handler inst labels machine pc flag stack ops)
  ;; instruction destruction
  (let ((dest (branch-dest inst)))
    ;; must be a label (might extend to "goto"
    ;; but this functionality is not seen in the book)
    (if (label-exp? dest)
        (let ((insts
               (lookup-label
                labels
                (label-exp-label dest))))
          (lambda ()
            ;; check the flag first and then make the decision
            ;; of either jumping or advancing pc
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        ;; just wondering why we need to print out these
        ;; error info while the error only happens when
        ;; being used internally.
        (error "BRANCH: bad instruction: " inst))))
(set-handler 'branch branch-handler)

(define (goto-handler inst labels machine pc flag stack ops)
  'todo)

(define (save-handler inst labels machine pc flag stack ops)
  'todo)

(define (restore-handler inst labels machine pc flag stack ops)
  'todo)

(define (perform-handler inst labels machine pc flag stack ops)
  'todo)
