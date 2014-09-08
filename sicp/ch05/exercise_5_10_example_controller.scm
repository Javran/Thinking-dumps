(define example-controller
  '(controller
    (copy a (const 10))
    (copy b (const 20))
    (call c + (reg a) (reg b) (const 30))))
