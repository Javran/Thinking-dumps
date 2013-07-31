(define (install-integer-package)
  ;; constructors
  (define (make data)
    (attach-tag 'integer data))

  ;; accessors
  ; (N/A)

  ;; math operations
  ; using system procedures to impl
  ;   add/sub/mul/equ?
  (define (=zero? x)
    (< (abs x) eps))

  ;; coercion system
  ; no way to project
  (define (raise x)
    ((get 'make 'rational) x 1))

  ;; visualization
  (define (print-num x)
    (display "integer: ")
    (display x))

  ;; installation
  (put 'make 'integer make)
  (put 'add '(integer integer) (tagged 'integer +))
  (put 'sub '(integer integer) (tagged 'integer -))
  (put 'mul '(integer integer) (tagged 'integer *))
  (put 'equ? '(integer integer) =)
  (put '=zero? 'integer =zero?)
  (put 'raise 'integer raise)
  (put 'print-num 'integer print-num)

  'done)

(define (test-integer-package)
  (display "Testing integer package ")
  (let* ((make (get 'make 'integer))
         (a (make 10))
         (b (make 2)))
    (let ((testcases (list
                       (cons (list 'add a b) ; 10 + 2 = 12
                             (make 12))
                       (cons (list 'sub a b) ; 10 - 2 = 8
                             (make 8))
                       (cons (list 'mul a b) ; 10 * 2 = 20
                             (make 20))
                       (cons (list 'equ? a b) ; 10 != 2
                             #f)))
          (f (lambda args
               (apply apply-generic args))))
      (do-test f testcases))))
