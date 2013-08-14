(load "../common/utils.scm")
(load "../common/test-utils.scm")
(load "./tag_system.scm")
(load "./number_system.scm")

(load "./5_3_polynominal_package.scm")
(install-polynomial-package)

(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (equal? v1 v2)))

(load "./5_3_poly_term_package.scm")
(load "./5_3_poly_termlist_package.scm")
(install-poly-term-package)
(install-poly-termlist-package)

(define make-term (get 'make 'poly-term))
(define (order x) (apply-generic 'order x))
(define (coeff x) (apply-generic 'coeff x))

(run-test 'poly-term-package)

(define make-termlist-from-args
  (get 'make-from-args 'poly-termlist))
(define make-poly
  (get 'make 'polynominal))
(define the-empty-term-list
  (get 'make 'poly-termlist))

(let ((l1 (make-termlist-from-args
            2 (make-scheme-number 3)
            1 (make-scheme-number 4)))
      (l2 (make-termlist-from-args
            1 (make-scheme-number 2)
            0 (make-scheme-number 2))))
  ; l1 = 3x^2+4x
  ; l2 = 2x+2
  (let ((p1 (make-poly 'x l1))
        (p2 (make-poly 'x l2)))
    (out (to-string (add p1 p2)))
    (out (to-string (mul p1 p2)))))
(newline)

(let ((l1 (make-termlist-from-args
            2 (make-scheme-number 3)
            1 (make-complex-ri 2 3)
            0 (make-scheme-number 7)))
      (l2 (make-termlist-from-args
            4 (make-scheme-number 1)
            2 (make-rational 2 3)
            0 (make-complex-ri 5 3))))
  (let ((p1 (make-poly 'x l1))
        (p2 (make-poly 'x l2)))
    (for-each
      (compose out to-string)
      (list p1 p2 (add p1 p2) (mul p1 p2)))))
(newline)

(let ((py1 (make-poly
             'y
             (make-termlist-from-args ; y + 1
               1 (make-scheme-number 1)
               0 (make-scheme-number 1))))
      (py2 (make-poly
             'y
             (make-termlist-from-args ; y^2 + 1
               2 (make-scheme-number 1)
               0 (make-scheme-number 1))))
      (py3 (make-poly
             'y
             (make-termlist-from-args ; y - 1
               1 (make-scheme-number 1)
               0 (make-scheme-number -1))))
      (py4 (make-poly
             'y
             (make-termlist-from-args ; y - 2
               1 (make-scheme-number 1)
               0 (make-scheme-number -2))))
      (py5 (make-poly
             'y
             (make-termlist-from-args ; y^3 + 7
               3 (make-scheme-number 1)
               0 (make-scheme-number 7)))))
  (let ((px1 (make-poly
               'x
               (make-termlist-from-args
                 2 py1
                 1 py2
                 0 py3)))
        (px2 (make-poly
               'x
               (make-termlist-from-args
                 1 py4
                 0 py5))))
    (for-each
      (compose out to-string)
      (list px1 px2 (mul px1 px2)))))
(newline)

; now we can make Pascal's triangle :)
(let ((t (make-poly
           'x
           (make-termlist-from-args ; x + 1
             1 (make-scheme-number 1)
             0 (make-scheme-number 1))))
      (one (make-poly
             'x
             (make-termlist-from-args 
               0 (make-scheme-number 1)))))
  (for-each
      (compose
        ; 6. output
        out
        ; 5. apply generic to-string
        ((curry2 map) to-string)
        ; 4. get the list of coeff
        ((curry2 apply-generic) 'coeff-list)
        ; 3. get term-list
        ((curry2 apply-generic) 'term-list))
    (map (lambda (len) 
           ; 2. accumulate by mul
           (fold-left 
             mul 
             one
             ; 1. make a list of length len, whose elements are all `t`
             (map 
               (const t)
               (list-in-range 1 len))))
         (list-in-range 0 10))))

(end-script)
