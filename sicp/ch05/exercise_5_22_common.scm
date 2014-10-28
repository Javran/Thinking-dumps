(define (my-append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))

;; require "x" to be an non-empty list
(define (my-last-pair x)
  (if (null? (cdr x))
      x
      (my-last-pair (cdr x))))

;; require "x" to be an non-empty list
(define (my-append! x y)
  (set-cdr! (my-last-pair x) y)
  x)

(define (test-my-append-machine
         my-append
         my-append-machine)
  (do-test
   (lambda (x y)
     (equal? (my-append x y)
             (my-append-machine x y)))
   (list
    (mat '(1 2 3) '(4 5 6) #t)
    (mat '(1 2) '(3) #t)
    (mat '(1 2) '() #t))))

(define (test-my-append!-machine
         my-append!
         my-append!-machine)
  ;; my-append! is like "my-append"
  (do-test
   (lambda (x y)
     ;; be careful about mutability
     (let ((x1 (tree-copy x))
           (y1 (tree-copy y))
           (x2 (tree-copy x))
           (y2 (tree-copy y)))
       (equal? (my-append x1 y1)
               (my-append-machine x2 y2))))
   (list
    (mat '(1 2 3) '(4 5 6) #t)
    (mat '(1 2) '(3) #t)
    (mat '(1 2) '() #t)))
  (define (append-twice-test my-append!)
    ;; be careful that "my-append!"
    ;; modifies the data,
    ;; so we'd better not used quoted constants
    (let ((x (list 1 2 3))
          (y (list 4 5 6))
          (z (list 7 8 9)))
      (assert (equal? (my-append! x y)
                      '(1 2 3 4 5 6)))
      (assert (equal? (my-append! y z)
                      '(4 5 6 7 8 9)))
      (assert (equal? x
                      '(1 2 3 4 5 6 7 8 9)))))
  (append-twice-test my-append!)
  (append-twice-test my-append!-machine))

