(load "../common/utils.scm")

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) 
            (* r (cos a)))
          ((eq? op 'imag-part)
            (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

(let ((x (make-from-mag-ang 10 (/ pi 6))))
  (out (x 'real-part) ; ~= (* (sqrt 3) 5)
       (x 'imag-part) ; ~= 5
       (x 'magnitude)
       (x 'angle)
       ))

(end-script)
