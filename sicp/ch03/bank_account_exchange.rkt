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

; exchange the balance of two accounts
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
            ; sort serializers using the proper comparator
            (sort
              (map (lambda (x) (x 'serializer))
                   args)
              serializer-less-than?))
          (serializer->serialize
            ; convert a serializer to a function
            ;   that does its job
            (lambda (s)
              (lambda (p)
                (serializer-proc s p)))))
      ; run the operation using the argument given
      (apply
        ; apply all serializers in order
        (apply-functions
          (map serializer->serialize 
               sorted-serializers)
          op)
        args))))

(define serialized-exchange
  (serialize-account-operation exchange))

; generate non-duplicate random integers
;   ranged in [a,b]
(define (generate-non-dup-rnd-ints n a b)
  (if (= n 0)
    '()
    (let ((vals (generate-non-dup-rnd-ints (- n 1) a b))
          (next-val (lambda () (+ a (random (+ (- b a) 1)))))
          ; a + [0,(b-a+1-1)] -> [a,b]
          )
      (cons
        (let loop ((val (next-val)))
          (if (memq val vals)
            ; re-run
            (loop (next-val))
            val))
        vals))))

(define (test1)
  (out "==== test #1 ====")
  (define acc1 (make-account 100))
  (define acc2 (make-account 400))

  (out "before:"
       (list (acc1 'balance) (acc2 'balance)))

  ; spawn 100 threads,
  ;   for each thread, do it 100 times
  (define threads
    (map
      (lambda (x) (thread (lambda () (do-test 100))))
      (build-list 100 (lambda (x) 1))))

  (define (do-test test-round)
    (if (= test-round 0)
      'done
      (begin
        (serialized-exchange acc1 acc2)
        (do-test (- test-round 1)))))

  (for-each
    thread-wait
    threads)

  (out "after:"
       (list (acc1 'balance) (acc2 'balance)))
  'done
  )

(define (test2)
  (out "==== test 2 ====")
  (define bank-accounts
    (map
      (lambda (x)
        ; make accounts that has a balance ranged [1..100]
        (make-account (+ 1 (random 100))))
      (build-list 100 (lambda (x) 1))))
  ; store the balances before anything is done
  (define balances-before
    (map (lambda (acc) (acc 'balance))
         bank-accounts))

  (define (do-test)
    (define (do-exchange-on account-idxs)
      ; ... don't know what happened
      ; I did lots of tests
      ; but changing this to `exchange`
      ; makes no difference
      ; anyway, just want to show that my implementation
      ; can pass the tests
      (serialized-exchange
        (list-ref bank-accounts (car account-idxs))
        (list-ref bank-accounts (cadr account-idxs))))
    (define todo-list
      (map
        (lambda (x)
          ; account index: [0,99]
          (generate-non-dup-rnd-ints 2 0 99))
        (build-list 200 (lambda (x) 1))))
    (for-each
      (lambda (todo)
        (do-exchange-on todo))
      todo-list))

  ; spawn 100 threads,
  ;   for each thread, simply run `do-test`
  (define threads
    (map
      (lambda (x) (thread (lambda () (do-test))))
      (build-list 100 (lambda (x) 1))))

  (for-each
    thread-wait
    threads)

  (define balances-after
    (map (lambda (acc) (acc 'balance))
         bank-accounts))

  (out "before:" balances-before
       "after:"  balances-after
       "correctness: "
         (equal? (sort balances-before <)
                 (sort balances-after  <)))
  )

(test1)
(test2)

; TODO:
; * multiple account exchange -> balance-cycle
