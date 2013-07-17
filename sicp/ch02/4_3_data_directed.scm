(load "../common/utils.scm")

; dispatching type: 
;   checking the type of a datum and calling an appropriate procedure

; data-directed programming:
;   deal with a two dimension table:
;   * one axis for possible operations
;   * one axis for possible types
; to add a new representation doesn't need to change any existing procedures
(load "./4_3_data_directed_put_get.scm")

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types: APPLY-GENERIC"
          (list op type-tags))))))

(load "./4_3_data_directed_ben_impl.scm")
(load "./4_3_data_directed_aly_impl.scm")

(install-rect-package)
(install-polar-package)

(pretty-print-proc-table)

(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))

(define make-from-real-imag
  (get 'make-from-real-imag 'rect))
(define make-from-mag-ang
  (get 'make-from-mag-ang 'polar))
(newline)

; excerpted tests from "./4_2_tagged_data.scm"
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
  (out c1 ; (rect 3 . 4)
       c2 ; (polar 3 . 4)
       (magnitude c1) ; 5
       (magnitude c2) ; 3
       ))

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
