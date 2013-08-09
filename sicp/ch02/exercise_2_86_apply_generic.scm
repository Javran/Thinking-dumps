; excerpted from ex 2.85

(load "./exercise_2_84_type_cmp.scm")
; raise data to a higher type (curried)
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

; drop
(load "./exercise_2_86_drop.scm")

; apply-generic without drop
(define (apply-generic-inner op . args)
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

(define (apply-generic-d op . args)
  (let ((result (apply apply-generic-inner (cons op args))))
    (if (non-empty? result)
      (let ((type (type-tag result)))
        ; make sure complex2 should not be involved in simplication 
        ; (actually complex2 and complex can be converted to each other
        ;  but we keep `drop` out side of complex2 so we can easily
        ;  figure out what happened)
        (cond ((equal? type 'complex2) result)
              (else (drop result))))
      result)))

(define apply-generic apply-generic-inner)
