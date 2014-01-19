#lang racket

(require "../common.rkt")

; ==== part 1 ====

(define y
  (lambda (f)
    (lambda (x)
      ((f (y f)) x))))

(let ((pfactorial
        ; the "almost" factorial 
        (lambda (f)
          (lambda (n)
            (if (= n 0)
              1
              (* n (f (- n 1)))))))
      (ptimes4
        ; the "almost" times4
        (lambda (f)
          (lambda (n)
            (if (= n 0)
              0
              (+ (f (- n 1)) 4))))))
  ; now the magic happens
  (let ((factorial (y pfactorial))
        (times4    (y ptimes4   )))
    (out (factorial 10)
         (times4    25))))

(define result
  (let* ((pseudo-y
           (lambda (psy)
             (lambda (f)
               (lambda (x)
                 ((f ((psy psy) f)) x)))))
         (y2
           (lambda (x)
             ((pseudo-y pseudo-y) x)))
         (pseudo-fact
           (lambda (f)
             (lambda (n)
               (if (= n 0)
                 1
                 (* n (f (- n 1))))))))
    (list
      ((y pseudo-fact) 10)
      ((y2 pseudo-fact) 10)
      )
    ))

(display result)
