(define (handle-save body ms)
  (let* ((var (car body))
         (val (ms-reg-get var ms)))
    (ms-stack-push val ms)))

(define (handle-restore body ms)
  (let* ((var (car body))
         (val (ms-stack-top ms)))
    (ms-reg-set var val
                (ms-stack-pop ms))))

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
