#lang racket

(let ((makeeven
        (lambda (makerO)
          (lambda (makerE)
            (lambda (x)
              (if (= x 0)
                1
                (((makerO makerE) makerO) (- x 1)))))))
      (makeodd
        (lambda (makerE)
          (lambda (makerO)
            (lambda (x)
              (if (= x 0)
                0
                (((makerE makerO) makerE) (- x 1))))))))
  (let ((even
          (lambda (x)
            (((makeodd makeeven) makeodd) x)))
        (odd
          (lambda (x)
            (((makeeven makeodd) makeeven) x))))
    (for-each (lambda (x)
                (display (list x '=>
                           'e (even x)
                           'o (odd x)))
                (newline))
              '(0 1 2 3 4 5))))
