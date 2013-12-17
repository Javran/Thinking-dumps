#lang racket
; bank account `exchange` implementation:
;   generate some bank accounts and share them
;   among threads, each thread takes some takes of
;   exchanging the balance of two accounts.
;   when the program terminates,
;   we sort the original account balances
;   and the final account balances
;   they should be the same.

(define (out . args)
  (for-each
    (lambda (x) (display x) (newline))
    args))

; each serializer needs an unique id in our implementation
; doc: http://docs.racket-lang.org/reference/semaphore.html
(define (make-serializer id)
  (let ((sema (make-semaphore 1)))
    (define (serialize p)
      (define (serialized-p . args)
        (semaphore-wait sema)
        (let ((val (apply p args)))
          (semaphore-post sema)
          val))
      serialized-p)
    (define (dispatch m)
      (cond ((eq? m 'serialize) serialize)
            ((eq? m 'id) id)))
    dispatch))

(define (serializer-id s)
  (s 'id))
(define (serializer-proc s p)
  ((s 'serialize) p))

; it depends on the proper way of comparing id
;   here I assume it being integers.
(define (serializer-less-than? s1 s2)
  (< (serializer-id s1)
     (serializer-id s2)))

(define *max-id* 0)

; return the old of `*max-id*`,
;   while increasing its value by 1
(define (new-id)
  (let ((val *max-id*))
    (set! *max-id* (+ *max-id* 1))
    val))

(define (make-account balance)
  ; not serialized version
  (define (withdraw amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      (error "Insufficient funds")))
  ; not serialized version
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer (new-id))))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else
              (error "Unknown request: MAKE-ACCOUNT"
                     m))))
    dispatch))

(define (exchange acc1 acc2)
  (let ((difference (- (acc2 'balance)
                       (acc1 'balance))))
    ; a+(b-a) = b
    ((acc1 'deposit) difference)
    ; b-(b-a) = a
    ((acc2 'withdraw) difference)
    'done))

; (apply-functions (list f g h) proc)
; => (h (g (f proc)))
(define (apply-functions functions proc)
  (if (null? functions)
    proc
    (apply-functions
      (cdr functions)
      ((car functions) proc))))

(define (serialize-account-operation op)
  ; Assume all args are account objects
  (lambda args
    (let ((sorted-serializers
            (sort
              (map (lambda (x) (x 'serializer))
                   args)
              serializer-less-than?))
          (serializer->serialize
            (lambda (s)
              (lambda (p)
                (serializer-proc s p)))))
      (apply
        (apply-functions
          (map serializer->serialize 
               sorted-serializers)
          op)
        args))))

(define serialized-exchange
  (serialize-account-operation exchange))

(define acc1 (make-account 100))
(define acc2 (make-account 400))

(out (list (acc1 'balance) (acc2 'balance)))

(define threads
  (map
    (lambda (x) (thread (lambda () (do-test 99))))
    (build-list 100 (lambda (x) (+ x 1)))))

(define (do-test test-round)
  (if (= test-round 0)
    'done
    (begin
      (serialized-exchange acc1 acc2)
      (do-test (- test-round 1)))))

(for-each
  thread-wait
  threads)

(out (list (acc1 'balance) (acc2 'balance)))

; TODO:
; * multiple account exchange -> balance-cycle
; * more comments
