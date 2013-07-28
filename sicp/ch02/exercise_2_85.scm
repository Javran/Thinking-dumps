(load "../common/utils.scm")

; * we need to describe the linear relationship of types
;   so we'll be able to compare the type levels
; * after we come up with the comparator, we'll modify the apply-generic
;   for the final target:
;   * get a list of types
;   * pick up the highest type in the type list
;   * every argument needs to be raised to be of that type
;   * we don't need to raise beyond the highest one

(load "./4_3_data_directed_put_get.scm")
(load "./exercise_2_85_num_all.scm")

; type comparison
(load "./exercise_2_84_type_cmp.scm")

; installation
(define (add a b) (apply-generic 'add a b))
(define (sub a b) (apply-generic 'sub a b))
(define (raise x) (apply-generic 'raise x))
(define (=zero? x) (apply-generic '=zero? x))
(define (equ? x y) (=zero? (sub x y)))
(define (project x) (apply-generic 'project x))

(define eps 1e-7)

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-complex-package)

(define make-integer (get 'make 'integer))
(define make-rational (get 'make 'rational))
(define make-real (get 'make 'real))
(define make-complex (get 'make 'complex))

; raise to a higher type for data (currying)
(define (raise-to type)
  (lambda (data)
    (let ((data-type (type-tag data)))
      (cond ((equal? type data-type) data)
            ((higher-type? type data-type)
             ((raise-to type) (raise data)))
            (else (error "no way to raise downwards: RAISE-TO"))))))

; pick up the highest type in the list
(define (highest-type ls)
  (fold-left
    (lambda (cur-highest cur-type)
      (if (higher-type? cur-type cur-highest)
        cur-type
        cur-highest))
    (car ls)
    (cdr ls)))

(define (apply-generic op . args)
  (define (raise-and-apply op args)
    (let* ((type-list (map type-tag args))
           (data-list (map contents args))

           (type-target (highest-type type-list))
           (new-type-list (map (const type-target) type-list))
           (raised-data (map (raise-to type-target) args))
           (proc (get op new-type-list)))
      (if proc
        (apply proc (map contents raised-data))
        (error "No method for thest types: APPLY-GENERIC"
               (list op args)))))

  (let ((type-list (map type-tag args))
        (data-list (map contents args)))
    (let ((proc (get op type-list)))
      (if proc
        (apply proc data-list)
        ; else try to raise all types and look for a procedure again
        (raise-and-apply op args)))))

(define (test-equ)
  (let ((x1 (make-integer 1))
        (x2 (make-rational 9 27))
        (x3 (make-real 14.2857))
        (x4 (make-complex 1 2)))
    (out (equ? x1 (make-integer 2)) ; #f
         (equ? x2 (make-rational 1 3)) ; #t
         (equ? x3 (make-real 1.42857e1)) ; #t
         (equ? x4 (make-complex 1 2.1)) ; #f
         )))

(define (test-project)
  (let ((x1 (make-rational 27 7))
        (x2 (make-real 45.875))
        (x3 (make-complex 2 3)))
    (apply out 
           (map project
                (list
                  x1 ; 4
                  x2 ; 367 8
                  x3 ; 2
                  )))))

; uncomment next lines for test
; (test-equ)
; (test-project)

(end-script)
