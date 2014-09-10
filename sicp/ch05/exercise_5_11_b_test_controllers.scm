(define controller-fine
  ;; this controller will be fine
  '(controller
    (assign a (const 1))
    (assign b (const 2))
    (save a)
    (save b)
    (assign a (const 10))
    (assign b (const 20))
    (restore b)
    (restore a)))

(define controller-fail
  ;; this controller will fail
  ;; if we implement the requirement
  ;; of ex 5.11.b correctly.
  '(controller
    (assign a (const 1))
    (assign b (const 2))
    (save a)
    (save b)
    (assign a (const 10))
    (assign b (const 20))
    (restore a) ;; executing this part will raise an error
    (restore b)))
