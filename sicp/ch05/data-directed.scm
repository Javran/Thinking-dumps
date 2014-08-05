(define (global-table-functions)
  (let ((*table* '()))
    (define (initialize!)
      (set! *table* '()))

    (define (get key)
      (let ((result (assoc key *table*)))
        (if result
            (cadr result)
            #f)))

    (define (set key val)
      (set! *table*
            (cons (list key val)
                  (del-assoc key *table*))))

    (list (list 'get get)
          (list 'set set)
          (list 'init initialize!))))

(define (data-directed-functions)
  (let* ((f-alist (global-table-functions))
         (initialize! (cadr (assoc 'init f-alist)))
         (get-intern (cadr (assoc 'get f-alist)))
         (set-intern (cadr (assoc 'set f-alist))))
    (define (get k1 k2)
      (get-intern (list k1 k2)))

    (define (set k1 k2 val)
      (set-intern (list k1 k2) val))

    (list (list 'get get)
          (list 'set set)
          (list 'init initialize!))))
