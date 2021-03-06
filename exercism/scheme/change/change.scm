(import (rnrs))

(use-modules ((srfi srfi-1) #:select (filter-map reduce)))

(define (change amount coins)
  (if (< amount 0)
      (raise 'negative-amount)
      (let ([;; memoize result for (search _) calls
             memo
             (make-vector (+ amount 1) 'unassigned)])
        (define (search target)
          (cond
           [(zero? target) '()]
           [(< target 0) #f]
           [else
            (or
             (let ([cached (vector-ref memo target)])
               (and (not (eq? cached 'unassigned))
                    cached))
             (let* ([results
                     (filter-map
                      (lambda (coin)
                        (let ([sub (search (- target coin))])
                          (and sub (cons coin sub))))
                      coins)]
                    [best-result-pair
                     (reduce
                      (lambda (e cur-best) (if (< (cdr e) (cdr cur-best)) e cur-best))
                      #f
                      (map (lambda (x) (cons x (length x))) results))])
               (let ([best (and best-result-pair (car best-result-pair))])
                 (vector-set! memo target best)
                 best)))]))
        (or (search amount)
            (raise 'impossible)))))
