(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1h (head s1))
                (s2h (head s2)))
            (let ((s1hw (weight s1h))
                  (s2hw (weight s2h)))
              (cond ((< s1hw s2hw)
                      (cons-stream
                        s1h
                        (merge-weighted (tail s1) s2 weight)))
                    ((> s1hw s2hw)
                      (cons-stream
                        s2h
                        (merge-weighted s1 (tail s2) weight)))
                    (else
                      (cons-stream
                        s1h
                        (merge-weighted
                          (tail s1)
                          (tail s2)
                          weight)))))))))

(define x (cons-stream 1 (scale-stream x 2)))
(define y (cons-stream 1 (scale-stream y 3)))

(print-few 10 (merge-weighted x y identity))

(end-script)
