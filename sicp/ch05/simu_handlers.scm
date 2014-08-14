(load "./data-directed.scm")

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

;; analyze the list of instructions once for all,
;; yielding "thunks" which can be used multiple times
;; without too much time consumption.

;; handler type:
;; (<handler> inst labels machine pc flag stack ops)
(define (assign-handler inst labels machine pc flag stack ops)
  ;; TODO: wouldn't it be a better idea to let
  ;; instruction handlers destruct the instruction in question?
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp
         (assign-value-exp inst)))
    (let ((value-proc
           ;; yields a value when run as a procedure
           (if (opeartion-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()
        ;; execute an operation or fetch register or constants according
        ;; to the instruction, advance "pc"
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (test-handler inst labels machine pc flag stack ops)
  ;; instruction destruction
  (let ((condition (test-condition inst)))
    (if (opeartion-exp? condition)
        (let ((condition-proc
               (make-opeartion-exp
                condition machine labels operations)))
          (lambda ()
            ;; execute the "operation", and then set the flag
            (set-contents! flag (condition-proc))
            (advance-pc)))
        ;; original error message is too verbose
        (error "TEST: bad instruction: "
               inst))))

(define (branch-handler inst labels machine pc flag stack ops)
  'todo)

(define (goto-handler inst labels machine pc flag stack ops)
  'todo)

(define (save-handler inst labels machine pc flag stack ops)
  'todo)

(define (restore-handler inst labels machine pc flag stack ops)
  'todo)

(define (perform-handler inst labels machine pc flag stack ops)
  'todo)
