(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1h (head s1))
                (s2h (head s2)))
            (let ((s1hw (weight s1h))
                  (s2hw (weight s2h)))
              (cond ((<= s1hw s2hw)
                      (cons-stream
                        s1h
                        (merge-weighted (tail s1) s2 weight)))
                    ((> s1hw s2hw)
                      (cons-stream
                        s2h
                        (merge-weighted s1 (tail s2) weight)))))))))

(define (weighted-pairs s t weight)
  ; assumption: 
  ;   (w (list (head s) (head t))) has the lowest value
  (cons-stream
    (list (head s) (head t))
    (merge-weighted
      (stream-map (lambda (x)
                    (list (head s) x))
                  (tail t))
      (weighted-pairs (tail s) (tail t) weight)
      weight)))
