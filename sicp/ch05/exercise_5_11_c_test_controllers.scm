(define test-controller-success-1
  ;; should have a = 10, b = 20 when done
  '(controller
    (assign a (const 10))
    (assign b (const 20))
    (save a)
    (save b)
    (assign a (const 40))
    (assign b (const 50))
    (restore a)
    (restore b)))

(define test-controller-success-2
  ;; should have a = 10, b = 20 when done
  '(controller
    (assign a (const 10))
    (save a)
    (assign b (const 20))
    (save b)
    (assign a (const 30))
    ;; "a" can be restored successfully
    ;; which is not possible in ex 5.11.b
    (restore a)))

(define test-controller-failure
  '(controller
    (assign a (const 1))
    (save a)
    (assign a (const 2))
    (save a)
    ;; raise error here
    ;; since stack for "b" is empty
    (restore b)
    (restore b)))
