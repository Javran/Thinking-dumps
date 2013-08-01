(define (install-real-package)
  ;; constructors
  (define make identity)

  ;; accessors
  ; (N/A)

  ;; math operations
  (define (=zero? x)
    (< (abs x) eps))

  (define (equ? x y)
    (=zero? (- x y)))

  ;; coercion system
  (define (raise x)
    ((get 'make 'complex) x 0))

  (define (project x)
    (define (real->rational n d target)
      (if (< (abs (- (/ (round n) (round d)) target)) eps)
        ((get 'make 'rational) (round n) (round d))
        (real->rational (* 10 n) (* 10 d) target)))
    (real->rational x 1 x))

  ;; visualization
  (define (print-num x)
    (display "real: ")
    (display x))

  ;; installation
  (put 'make 'real (tagged 'real make))
  (put 'add '(real real) (tagged 'real +))
  (put 'sub '(real real) (tagged 'real -))
  (put 'mul '(real real) (tagged 'real *))
  (put 'equ? '(real real) equ?)
  (put '=zero? 'real =zero?)
  (put 'raise 'real raise)
  (put 'project 'real project)
  (put 'print-num 'real print-num)
  'done)

(define (test-real-package)
  (display "Testing real package ")
  (let* ((make (get 'make 'real))
         (a (make 98.76))
         (b (make 12.34)))
    (let ((testcases (list
                       (cons (list 'add a b)
                             (make (+ 98.76 12.34)))
                       (cons (list 'sub a b)
                             (make (- 98.76 12.34)))
                       (cons (list 'mul a b)
                             (make (* 98.76 12.34)))
                       (cons (list 'equ? a b)
                             #f)))
          (f (lambda args
               (apply apply-generic args))))
      (do-test f testcases))))
