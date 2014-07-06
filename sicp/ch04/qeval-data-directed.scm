;; generate a predicate to test if a given data
;; is tagged with a specified tag.
(define (list-tagged-with tag)
  (lambda (l)
    (and
      (list? l)
      (non-empty? l)
      (eq? (car l) tag))))

;; a special form is identified by the "car" part
;; of it, which is called "type".
;; and rest of the data is the "contents"
(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE"
             exp)))
(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS"
             exp)))

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

(define (qeval-data-directed-tests)
  ;; test "list-tagged-with"
  (do-test
   (list-tagged-with 'tag)
   (list
    (mat '(tag1 tag) #f)
    (mat '() #f)
    (mat #f #f)
    (mat '(tag . data) #f)
    (mat '(tag data) #t)))
  ;; test "type" and "contents"
  (do-test
   type
   (list
    (mat '(a b) 'a)
    (mat '(ty cont) 'ty)))
  (do-test
   contents
   (list
    (mat '(a b) '(b))
    (mat '(ty cont) '(cont))))
  (proc-table-initialize!)
  ;; test "get" and "put"
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
    (qeval-data-directed-tests)
    'ok)

;; testcases might modify the table,
;; so we might need to initialize it here.
(proc-table-initialize!)
;; Local variables:
;; proc-entry: "qeval.scm"
;; End:
