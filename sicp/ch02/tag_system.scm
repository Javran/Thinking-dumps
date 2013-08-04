(load "../common/utils.scm")
(load "../common/test-utils.scm")

; tag and datum
;   * attach-tag
;   * type-tag
;   * contents

; procedure
;   * put
;   * get

; apply
;   * apply-generic

; ==== implementations ====

(define attach-tag cons)
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum: CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types: APPLY-GENERIC"
          (list op type-tags))))))

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

(define (get-proc op type table)
  ; use assoc because type-val would be a list of symbols
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

; ==== tests ====
(define (test-tag-system)
  ; test tag
  (let* ((x (attach-tag 'tag 'data))
         (f (lambda (proc) (proc x))))
    (do-test-q f (list (mat type-tag 'tag)
                       (mat contents 'data))))

  ; test type
  (let ((origin-table proc-table))
    (set! proc-table nil)

    (assert (not (get 'op1 '(tag))))
    (assert (not (get 'op2 '(tag tag))))

    (put 'op1 '(tag) identity)
    (put 'op2 '(tag tag) cons)

    (assert (get 'op1 '(tag)))
    (assert (get 'op2 '(tag tag)))

    (let ((x1 (attach-tag 'tag 'data))
          (x2 (attach-tag 'tag 'data2)))
      (assert (equal? 'data (apply-generic 'op1 x1)))
      (assert (equal? '(data . data2) (apply-generic 'op2 x1 x2))))
    (set! proc-table origin-table)))

(test-tag-system)
