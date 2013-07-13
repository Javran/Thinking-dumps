(load "../common/utils.scm")

; dispatching type: 
;   checking the type of a datum and calling an appropriate procedure

; data-directed programming:
;   deal with a two dimension table:
;   * one axis for possible operations
;   * one axis for possible types
; to add a new representation doesn't need to change any existing procedures

; a 2d table recording the corresponding proc:
; proc-table[type][op] => the proc
(define proc-table nil)

(define (put-alist key val alist)
  (cons (list key val)
        (del-assq key alist)))

(define (put-proc op type item table)
  (let ((type-val (assq type table)))
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

; tests:
(put 'add 'tp1 'proc1) ; <- overridden by proc4
(put 'sub 'tp1 'proc2)
(put 'add 'tp2 'proc3)
(put 'add 'tp1 'proc4)

(out proc-table)

(end-script)
