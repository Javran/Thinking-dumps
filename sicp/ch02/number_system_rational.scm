(define (install-rational-package)

  (define (make n d)
    (assert (and (integer? n)
                 (integer? d))
            "rational should be made from integers")
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (define numer car)
  (define denom cdr)

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
  (define (inv x)
    (make (denom x) (numer x)))
  (define (div x y)
    (mul x (inv y)))

  (define (=zero? x)
    (= (numer x) 0))

  (define (equ? x y)
    (=zero? (sub x y)))

  (define (to-string x)
    (string-append
      (number->string (numer x))
      "/"
      (number->string (denom x))))

  (define (raise x)
    ((get 'make 'scheme-number)
      (/ (numer x) (denom x))))

  (define (test-rational)
    (let* ((make (get 'make 'rational))
           (x1 (make 1 7))
           (x2 (make 8 56))
           (x3 (make 4 8))
           (make-scheme-number (get 'make 'scheme-number))
           (num-equ? (lambda (a b) (apply-generic 'equ? a b))))
      ; test equ? and =zero?
      (let ((testcases
              (list (mat '=zero? (make 0 1) #t)
                    (mat '=zero? (make 1 1) #f)
                    (mat 'equ? x1 x2 #t)
                    (mat 'equ? x1 x3 #f)
                    (mat 'equ? x3 x2 #f))))
        (do-test-q apply-generic testcases))
      ; test accessors
      (let ((testcases
              (list (mat 'numer x1 1)
                    (mat 'numer x2 1)
                    (mat 'denom x1 7))))
        (do-test-q apply-generic testcases =))
      ; test math
      (let ((testcases
              (list (mat 'add x1 x2 (make 2 7))
                    (mat 'sub x3 x1 (make 5 14))
                    (mat 'mul x1 x3 (make 1 14))
                    (mat 'div x3 x1 (make 7 2)))))
        (do-test-q apply-generic testcases num-equ?))
      ; test raise
      (let ((testcases
              (list (mat 'raise (make 20 10) (make-scheme-number 2))
                    (mat 'raise (make 1 7) (make-scheme-number (/ 1 7))))))
        (do-test-q apply-generic testcases num-equ?))
      ; test to-string
      (let ((testcases
              (list (mat 'to-string x1 "1/7")
                    (mat 'to-string x2 "1/7")
                    (mat 'to-string x3 "1/2"))))
        (do-test-q apply-generic testcases))
      ))

  (put 'make 'rational (tagged 'rational make))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'add '(rational rational) (tagged 'rational add))
  (put 'sub '(rational rational) (tagged 'rational sub))
  (put 'mul '(rational rational) (tagged 'rational mul))
  (put 'div '(rational rational) (tagged 'rational div))
  (put '=zero? '(rational) =zero?)
  (put 'equ? '(rational rational) equ?)
  (put 'to-string '(rational) to-string)
  (put 'raise '(rational) raise)
  (put 'test 'rational-package test-rational)
  'done)

