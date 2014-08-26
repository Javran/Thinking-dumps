(load "./data-directed.scm")

(define set-handler #f)
(define get-handler #f)
(define initialize-handler-table! #f)

(let* ((f-alist (global-table-functions))
       (set1 (cadr (assoc 'set f-alist)))
       (get1 (cadr (assoc 'get f-alist)))
       (init1 (cadr (assoc 'init f-alist))))
  (set! set-handler set1)
  (set! get-handler get1)
  (set! initialize-handler-table! init1))

(initialize-handler-table!)

(define (handle-save body ms)
  (let* ((var (car body))
         (val (ms-reg-get var ms)))
    (ms-stack-push val ms)))
(set-handler 'save handle-save)

(define (handle-restore body ms)
  (let* ((var (car body))
         (val (ms-stack-top ms)))
    (ms-reg-set var val
                (ms-stack-pop ms))))
(set-handler 'restore handle-restore)

(define (handle-goto body ms)
  (let ((type (caar body))
        (arg  (cadar body)))
    (let ((lbl
           (cond
            ((eq? type 'reg)
             (ms-reg-get arg ms))
            ((eq? type 'label)
             arg)
            (else
             (error "unknown type:"
                    type)))))
      (ms-insns-set
       (ms-query-label lbl ms)
       ms))))
(set-handler 'goto handle-goto)

(define (handle-branch body ms)
  (let ((type (caar body))
        (arg  (cadar body)))
    (let ((lbl
           (cond
            ((eq? type 'reg)
             (ms-reg-get arg ms))
            ((eq? type 'label)
             arg)
            (else
             (error "unknown type:"
                    type)))))
      (if (ms-test-flag ms)
          (ms-insns-set
           (ms-query-label lbl ms)
           ms)
          ms))))
(set-handler 'branch handle-branch)

(define (handle-test body ms)
  (let ((type (caar body))
        (arg  (cadar body))
        (args (cdr body)))
    (define (get-value data)
      (let ((type (car data))
            (arg  (cadr data)))
        (cond
         ((eq? type 'const) arg)
         ((eq? type 'reg) (ms-reg-get arg ms))
         (else
          (error "unknown type"
                 type)))))
    (assert (eq? type 'op))
    (let ((operator (eval arg user-initial-environment))
          (operands (map get-value args)))
      (ms-test-flag-set
       (apply operator operands)
       ms))))
(set-handler 'test handle-test)

(define (handle-assign body ms)
  (let ((target (car body))
        (type (caadr body))
        (arg  (cadadr body))
        (args (cddr body)))
    (define (get-value data)
      (let ((type (car data))
            (arg  (cadr data)))
        (cond
         ((eq? type 'const) arg)
         ((eq? type 'reg) (ms-reg-get arg ms))
         (else
          (error "unknown type"
                 type)))))
    (let ((new-val
           (cond
            ((eq? type 'label) arg)
            ((eq? type 'const) arg)
            ((eq? type 'reg) (ms-reg-get arg ms))
            ((eq? type 'op)
             (let ((operator (eval arg user-initial-environment))
                   (operands (map get-value args)))
               (apply operator operands)))
            (else
             (error "unknown type"
                    type)))))
      (ms-reg-set target new-val ms))))
(set-handler 'assign handle-assign)
