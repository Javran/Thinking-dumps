(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./legacy-easy.scm")

(load "./exercise_5_12_analyze.scm")

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        ;; data path meta data stores
        ;; the analyzed data
        (data-path-meta '*unassigned*))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
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
      (define (data-path-meta-set data)
        (set! data-path-meta data))
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
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'data-path-meta-get)
               data-path-meta)
              ((eq? message 'data-path-meta-set)
               data-path-meta-set)
              (else
               (error "Unknown request: MACHINE"
                      message))))
      dispatch)))

(define (assemble controller-text machine)
  ;; add a dp-meta generation phrase at the beginning
  (let ((dp-meta (data-path-analyze controller-text)))
    ((machine 'data-path-meta-set) dp-meta)
    (extract-labels
     controller-text
     (lambda (insts labels)
       (update-insts! insts labels machine)
       insts))))

(load "./figure_5_12.scm")

(define (pretty-print-data-path-meta m)
  (pretty-print-data-path-analysis
   (m 'data-path-meta-get)))

(let ((m (make-with
          fib-machine-controller
          ;; not necessary since we don't need to execute it
          ;; but anyway
          '((n 5))
          (default-primitive-list))))
  (pretty-print-data-path-meta m))
