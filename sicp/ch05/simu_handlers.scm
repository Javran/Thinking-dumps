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
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()
        ;; execute an operation or fetch register or constants according
        ;; to the instruction, advance "pc"
        (set-contents! target (value-proc))
        (advance-pc pc)))))
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
