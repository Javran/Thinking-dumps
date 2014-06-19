;; procedures to be exposed
(define get nil)
(define put nil)
(define proc-table-initialize! nil)

;; initialize using let-expression,
;; making the variable only visible inside the implementation part.
(let ((proc-table nil))

  ;; update a key-value pair in an alist
  (define (put-alist key val alist)
    (cons (cons key val)
          (del-assoc key alist)))

  (define (put-proc key1 key2 proc table)
    ;; use the first key to find the subtable
    (let ((result (assoc key1 table)))
      ;; create an empty one if it does not exist
      (let ((subtable (if result (cdr result) nil)))
        (put-alist
         key1
         (put-alist
          key2
          proc
          subtable)
         table))))

  ;; assume "#f" can never be a valid value
  (define (get-proc key1 key2 table)
    (let ((result (assoc key1 table)))
      (if result
          (let* ((subtable (cdr result))
                 (result (assoc key2 subtable)))
            (if result
                (cdr result)
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

(define (test-get-put)
  (proc-table-initialize!)
  (do-test
   get
   (list
    (mat 'a 'b #f)
    (mat 'c 'd #f)))
  (put 'a 'b "ab")
  (put 10 20 "10-20")
  (put '(1 2 3) '(2 3 4) 'complex)
  (do-test
   get
   (list
    (mat 'a 'b "ab")
    (mat 'no-such 'value #f)
    (mat 10 20 "10-20")
    (mat '(1 2 3) '(2 3 4) 'complex)))
  'ok)

(if *qeval-tests*
    (test-get-put)
    'ok)

;; testcases might modify the table,
;; so we might need to initialize it here.
(proc-table-initialize!)
;; Local variables:
;; proc-entry: "qeval.scm"
;; End:
