(load "../common/utils.scm")

(define make-interval cons)
(define lower-bound car)
(define upper-bound cdr)

; pretty-print
(define (print-interval a)
  (display "[")
  (display (lower-bound a))
  (display "-")
  (display (upper-bound a))
  (display "]")
  (newline))

(define (mul-interval-1 x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; "nine cases" I think too bad to call it a hint.
; anyway ... I guess its 3 cases x 3 cases, now let's figure it out:
; inv -> interval
; lb -> lower-bound
; ub -> upper-bound

(define (mul-interval-2 x y)
  (let ((lb1 (lower-bound x))
        (ub1 (upper-bound x))
        (lb2 (lower-bound y))
        (ub2 (upper-bound y)))
    (cond 
      ((< ub1 0)
        (cond
          ((< ub2 0)
            ; case #1: inv 1 < 0, inv 2 < 0
            ; [inv 1]
            ; ----------0---------->
            ; [inv 2]
            ; result: [ub1*ub2 - lb1*lb2]
            (make-interval (* ub1 ub2) (* lb1 lb2)))

          ((> lb2 0)
            ; case #3: inv 1 < 0, inv 2 > 0
            ; [inv 1]
            ; ----------0---------->
            ;                [inv 2]
            ; result: [lb1*ub2 - ub1*lb2]
            (make-interval (* lb1 ub2) (* ub1 lb2)))
            
          (else 
            ; case #2: inv 1 < 0, inv 2 spans 0
            ; [inv 1]
            ; ----------0---------->
            ;        [inv 2]
            ; result: [lb1*ub2 - lb1*lb2]
            (make-interval (* lb1 ub2) (* lb1 lb2)))))

      ((> lb1 0)
        (cond
          ((< ub2 0)
            ; case #7:
            ;                [inv 1]
            ; ----------0---------->
            ; [inv 2]
            ; result: [ub1*lb2 - lb1*ub2]
            (make-interval (* ub1 lb2) (* lb1 ub2)))

          ((> lb2 0)
            ; case #9:
            ;                [inv 1]
            ; ----------0---------->
            ;                [inv 2]
            ; result: [lb1*lb2 - ub1*ub2]
            (make-interval (* lb1 lb2) (* ub1 ub2)))

          (else
            ; case #8:
            ;                [inv 1]
            ; ----------0---------->
            ;        [inv 2]
            ; result: [ub1*lb2 - ub1*ub2]
            (make-interval (* ub1 lb2) (* ub1 ub2)))))

      (else
        (cond
          ((< ub2 0)
            ; case #4:
            ;        [inv 1]
            ; ----------0---------->
            ; [inv 2]
            ; result: [ub1*lb2 - lb1*lb2]
            (make-interval (* ub1 lb2) (* lb1 lb2)))

          ((> lb2 0)
            ; case #6:
            ;        [inv 1]
            ; ----------0---------->
            ;                [inv 2]
            ; result: [lb1*ub2 - ub1*ub2] 
            (make-interval (* lb1 ub2) (* ub1 ub2)))

          (else
            ; case #5:
            ;        [inv 1]
            ; ----------0---------->
            ;        [inv 2]
            ; result: [min(lb1*ub2,ub1*lb2) - max(lb1*lb2,ub1*ub2)]
            (make-interval (min (* lb1 ub2) (* ub1 lb2))
                           (max (* lb1 lb2) (* ub1 ub2)))))))))


(define (test-mul x y)
  (print-interval (mul-interval-1 x y))
  (print-interval (mul-interval-2 x y)))

(test-mul (make-interval -10 -1)
          (make-interval -8 -2))

(test-mul (make-interval -10 2)
          (make-interval -4 14))

; should have same output for each case

; pick up nums in [-9 - 9], make intervals to test
(define (mul-test)
  (define (interval-eq? a b)
    (and (= (lower-bound a) (lower-bound b))
         (= (upper-bound a) (upper-bound b))))

  ; code here might be confusing ... 
  ; because loop & i & j are masked in inner scopes
  ; loop of 1st i
  (let loop ((i -9))
    (if (<= i 9)
      (begin
        ; loop of 1st j
        (let loop ((j i))
          (if (<= j 9)
            ; make interval-1
            (begin
              (let ((iv1 (make-interval i j)))
                ; loop of 2nd i
                (let loop ((i -9))
                  (if (<= i 9)
                    (begin
                      ; loop of 2nd j
                      (let loop ((j i))
                        (if (<= j 9)
                          ; make interval-2
                          (let ((iv2 (make-interval i j)))
                            (if (interval-eq? (mul-interval-1 iv1 iv2)
                                              (mul-interval-2 iv1 iv2))
                              ; next of 2nd j
                              (loop (+ j 1))
                              ; else
                              (begin
                                (out "error occurred:")
                                (display "iv1: ")
                                (print-interval iv1)
                                (display "iv2: ")
                                (print-interval iv2)
                                (out "results:")
                                (print-interval (mul-interval-1 iv1 iv2))
                                (print-interval (mul-interval-2 iv1 iv2))
                                (error "result mismatch"))))))
                      ; next of 2nd i
                      (loop (+ i 1))))))
              ; next of 1st j
              (loop (+ j 1)))))
        ; next of 1st i
        (loop (+ i 1))))))

; uncomment the next line for tests
; (mul-test)
