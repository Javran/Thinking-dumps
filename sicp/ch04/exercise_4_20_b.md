The procedure is:
    
    (define (f x)
      (let ((even?
              (lambda (n)
                (if (= n 0)
                  true
                  (odd?  (- n 1)))))
            (odd?
              (lambda (n)
                (if (= n 0)
                  false
                  (even? (- n 1))))))
      <rest of body of f>))

We can expand the multideclared let to
a nested structure:

    (define (f x)
      (let ((even?
              (lambda (n)
                (if (= n 0)
                  true
                  (odd?  (- n 1))))))
        (let ((odd?
                (lambda (n)
                  (if (= n 0)
                    false
                    (even? (- n 1))))))
          <rest of body of f>)))

Where we found that `even?` will not have chance to know
anything about `odd?`.
