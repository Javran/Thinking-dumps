(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (factorial-fp n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product) (+ counter 1))))
  (iter 1 1))

(define (factorial-ip n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
        product
        (begin
          (set! product (* counter product))
          (set! counter (+ counter 1))
          (iter))))
    (iter)))

(define (factorial-ip-bug n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
        product
        (begin
          ; we have to consider the relative order
          ;   of the assignment, this issue simply
          ;   does not arise in functional programs
          (set! counter (+ counter 1))
          (set! product (* counter product))
          (iter))))
    (iter)))

(out (map factorial-fp (list-in-range 1 10)))
(out (map factorial-ip (list-in-range 1 10)))
(out (map factorial-ip-bug (list-in-range 1 10)))

(end-script)
