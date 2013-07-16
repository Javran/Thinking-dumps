; a 2d table recording the corresponding proc:
; proc-table[type][op] => the proc
(define proc-table nil)

(define (put-alist key val alist)
  (cons (list key val)
        (del-assoc key alist)))

(define (put-proc op type item table)
  ; use assoc because type-val would be a list of symbols
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

(define (get op type)
  ; use assoc because type-val would be a list of symbols
  (let ((type-val (assoc type proc-table)))
    (if type-val
      (let* ((op-table (cadr type-val))
             (op-proc-pair (assoc op op-table)))
        (if op-proc-pair
          (cadr op-proc-pair)
          #f))
      #f)))

(define (get-put-tests)
  (let ((origin-table proc-table))
    ; tests:
    (put 'add 'tp1 'proc1) ; <- overridden by proc4
    (put 'sub 'tp1 'proc2)
    (put 'add 'tp2 'proc3)
    (put 'add 'tp1 'proc4)
    (put 'sub 'tp2 'proc5)
    (put 'add '(tp1 tp2) 'proc7)
    (put 'add '(tp1 tp2) 'proc8)
    (put 'sub '(tp1 tp2) 'proc9)
    (pretty-print-proc-table)

    (out (get 'add 'tp1)
         (get 'a 'b)
         (get 'c 'tp2)
         (get 'sub 'tp2))
    (set! proc-table origin-table)))

(define (pretty-print-proc-table)
  (define (pretty-print-type-entity pair)
    (display "type: ")
    (display (car pair))
    (newline)
    (for-each
      pretty-print-op-entity
      (cadr pair)))
  (define (pretty-print-op-entity pair)
    (display "  op: ")
    (display (car pair))
    (display " -> ")
    (display (cadr pair))
    (newline))
  (for-each
    pretty-print-type-entity
    proc-table))

; uncomment next line for tests
; (get-put-tests)
