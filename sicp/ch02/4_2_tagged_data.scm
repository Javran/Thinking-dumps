(load "../common/utils.scm")

(define attach-tag cons)
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum: CONTENTS" datum)))

(define (rect? z)
  (eq? (type-tag z) 'rect))

(define (polar? z)
  (eq? (type-tag z) 'polar))

; ben's impl
(define real-part-rect car)
(define imag-part-rect cdr)
(define (magnitude-rect z)
  (sqrt (+ (square (real-part-rect z))
           (square (imag-part-rect z)))))
(define (angle-rect z)
  (atan (imag-part-rect z)
        (real-part-rect z)))
(define (make-from-real-imag-rect x y)
  (attach-tag 'rect (cons x y)))
(define (make-from-mag-ang r a)
  (make-from-real-imag-rect (* r (cos a))
                            (* r (sin a))))

; alyssa's impl
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define magnitude-polar car)
(define angle-polar cdr)
(define (make-from-real-imag-polar x y)
  (make-from-mag-ang (sqrt (+ (square x) (square y)))
                     (atan y x)))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

; dispatch data to different impl according to its tag
; ... I think these procedures are somehow redundant
(define (real-part z)
  (cond ((rect? z)
          (real-part-rect (contents z)))
        ((polar? z)
          (real-part-polar (contents z)))
        (else (error "Unknown type: REAL-PART" z))))

(define (imag-part z)
  (cond ((rect? z)
          (imag-part-rect (contents z)))
        ((polar? z)
          (imag-part-polar (contents z)))
        (else (error "Unknown type: IMAG-PART" z))))

(define (magnitude z)
  (cond ((rect? z)
          (magnitude-rect (contents z)))
        ((polar? z)
          (magnitude-polar (contents z)))
        (else (error "Unknown type: MAGNITUDE" z))))

(define (angle z)
  (cond ((rect? z)
          (angle-rect (contents z)))
        ((polar? z)
          (angle-polar (contents z)))
        (else (error "Unknown type: ANGLE" z))))

(define make-from-real-imag make-from-real-imag-rect)
(define make-from-mag-ang make-from-mag-ang-polar)

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-mag-ang (- (real-part z1) (real-part z2))
                     (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(let ((c1 (make-from-real-imag 3 4))
      (c2 (make-from-mag-ang 3 4)))
  (out c1 ; underlying data structure of c1 and c2
       c2
       (magnitude c1) ; get magnitude
       (magnitude c2)))

(define (print-complex z)
  (display (real-part z))
  (display "+ ")
  (display (imag-part z))
  (display "i\n"))

(newline)
(let ((c1 (make-from-mag-ang (sqrt 2) (* pi (/ 3 4))))
      (c2 (make-from-real-imag 1 1)))
  ; c1 = -1+i
  ; c2 = 1+i
  (print-complex c1)
  (print-complex c2)
  (print-complex (add-complex c1 c2)) ;  0+2i
  (print-complex (sub-complex c1 c2)) ; -2+0i
  (print-complex (mul-complex c1 c2)) ; -2+0i
  (print-complex (div-complex c1 c2)) ;  0+1i
  )

(end-script)
