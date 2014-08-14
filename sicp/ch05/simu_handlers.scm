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

;; handler type:
;; (<handler> inst labels machine pc flag stack ops)
(define (assign-handler inst labels machine pc flag stack ops)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp
         (assign-value-exp inst)))
    (let ((value-proc
           (if (opeartion-exp? value-exp)
               (make-opeartion-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))


(define (test-handler inst labels machine pc flag stack ops)
  'todo)

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
