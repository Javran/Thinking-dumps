(load "../common/utils.scm")

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (add-terms (term-list p1)
                          (term-list p2)))
    (error "Polys not in same ver: ADD-POLY"
           (list p1 p2))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (mul-terms (term-list p1)
                          (term-list p2)))
    (error "Polys not in same ver: MUL-POLY"
           (list p1 p2))))

(end-script)
