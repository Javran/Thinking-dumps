(load "../common/utils.scm")

(define (gen-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (list-in-range 1 (- j 1))))
                      (list-in-range 1 (- i 1))))
           (list-in-range 1 n)))

(define (find-triples n s)
  (filter (lambda (t)
            (let ((a (car t))
                  (b (cadr t))
                  (c (caddr t)))
              (= s (+ a b c))))
          (gen-triples n)))

(out (find-triples 20 10))
; find all triples that 1<=k<j<i<=n
; ((5 3 2) (5 4 1) (6 3 1) (7 2 1))

(end-script)
