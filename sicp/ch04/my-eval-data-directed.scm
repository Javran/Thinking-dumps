; infrastructure for data-directed style

(define my-eval-get! #f)
(define my-eval-put! #f) 

(let ((eval-handler-alist '()))
  ; keep the handlers inside

  (define (put-proc op type item table)
    (let ((type-val (assoc type table)))
      (if type-val
        ; if the type exists
        (let* ((op-table (cadr type-val))
               (new-op-table (put-alist op item op-table)))
          (put-alist type new-op-table table))
        ; else
        (put-alist type
                   (list (list op item)) ; single key-value pair
                   table))))

  (define (put op type item)
    (set! proc-table (put-proc op type item proc-table)))

  (define (get-proc op type table)
    (let ((type-val (assoc type table)))
      (if type-val
        (let* ((op-table (cadr type-val))
               (op-proc-pair (assoc op op-table)))
          (if op-proc-pair
            (cadr op-proc-pair)
            #f))
        #f)))

(define (get op type)
  (get-proc op type proc-table))


  (set! my-eval-get! get)
  (set! my-eval-put! 'put)
  'done)

