;; procedures to be exposed
(define get nil)
(define put nil)
(define proc-table-initialize! nil)

;; initialize using let-expression,
;; making the variable only visible inside the implementation part.
(let ((proc-table nil))

  ;; update a key-value pair in an alist
  (define (put-alist key val alist)
    (cons (list key val)
          (del-assoc key alist)))

  (define (put-proc key1 key2 proc table)
    ;; use the first key to find the subtable
    (let ((subtable (assoc key1 table)))
      ;; create an empty one if it does not exist
      (let ((new-subtable (if subtable subtable nil)))
        (put-alist
         key1
         (put-alist
          key2
          proc
          new-subtable)
         table))))

  ;; assume "#f" can never be a valid value
  (define (get-proc key1 key2 table)
    (let ((subtable (assoc key1 table)))
      (if subtable
          (let ((subsubtable (assoc key2 subtable)))
            (if subsubtable
                (cdr subsubtable)
                #f))
          #f)))

  (define (put-impl key1 key2 val)
    (set! proc-table
          (put-proc key1 key2 val proc-table)))

  (define (get-impl key1 key2)
    (get-proc key1 key2 proc-table))

  (define (initialize)
    (set! proc-table nil))

  (set! put put-impl)
  (set! get get-impl)
  (set! proc-table-initialize! initialize)
  'ok)
