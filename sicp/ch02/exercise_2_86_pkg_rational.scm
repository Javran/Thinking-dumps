(define (install-rational-package)
  
  ;; constructor
  (define (make n d)
    (if (and (integer? n)
             (integer? d))
      (let ((g (gcd n d)))
        (cons (/ n g)
              (/ d g)))
      (error "arguments should all be integers: MAKE-RATIONAL"
             n d)))

  ;; accessors
  (define numer car)
  (define denom cdr)

  ;; math operations
  (define (add x y)
    (make (+ (* (numer x) (denom y))
             (* (numer y) (denom x)))
          (* (denom x) (denom y))))
  (define (neg x)
    (make (- (numer x))
          (denom x)))
  (define (sub x y)
    (add x (neg y)))
  (define (mul x y)
    (make (* (numer x) (numer y))
          (* (denom x) (denom y))))

  (define (=zero? x)
    (= (numer x) 0))

  (define (equ? x y)
    (=zero? (sub x y)))
  
  ;; coercion system
  (define (raise x)
    ((get 'make 'real) (/ (numer x) (denom x))))

  (define (project x)
    ((get 'make 'integer)
      (round (/ (numer x) (denom x)))))

  ;; visualization
  (define (print-num x)
    (display "rational: ")
    (display (numer x))
    (display "/")
    (display (denom x)))

  ;; installation
  (put 'make 'rational (tagged 'rational make))
  (put 'numer 'rational numer)
  (put 'denom 'rational denom)
  (put 'add '(rational rational) (tagged 'rational add))
  (put 'sub '(rational rational) (tagged 'rational sub))
  (put 'mul '(rational rational) (tagged 'rational mul))
  (put '=zero? 'rational =zero?)
  (put 'equ? '(rational rational) equ?)
  (put 'raise 'rational raise)
  (put 'project 'rational project)
  (put 'print-num 'integer print-num)

  'done)

(define (test-rational-package)
  (display "Testing rational package #1 ")
  (let* ((make (get 'make 'rational))
         (a (make 13 37))
         (b (make 23 70)))
    (let ((testcases (list
                       (cons (list 'add a b)
                             (make 1761 2590))
                       (cons (list 'sub a b)
                             (make 59 2590))
                       (cons (list 'mul a b)
                             (make 299 2590))
                       (cons (list 'equ? a a)
                             #t)))
          (f (lambda args
               (apply apply-generic args))))
      (do-test f testcases))
  (display "Testing rational package #2 ")
    (let ((testcases (list
                       (cons (list 'numer a)
                             13)
                       (cons (list 'denom b)
                             70)
                       (cons (list '=zero? a)
                             #f)))
          (f (lambda (op . args)
               (apply (get op 'rational) (map contents args)))))
      (do-test f testcases)))
  )

