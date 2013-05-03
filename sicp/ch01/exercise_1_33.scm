(load "../common/utils.scm")

; filtered-accumulate - recursive version
(define (accumulate-rec combiner null-value term a next b predicate)
  (if (> a b)
    null-value
    (let ((rest-results (lambda () 
                          (accumulate-rec
                            combiner
                            null-value 
                            term 
                            (next a) 
                            next 
                            b
                            predicate))))              
      (if (predicate a)
        (combiner (term a) (rest-results))
        (rest-results)))))

; filtered-accumulate - iteractive version
(define (accumulate-itr combiner null-value term a next b predicate)
  (define (accumulate-iter a acc)
    (if (> a b)
      acc
      (accumulate-iter (next a)
                       (if (predicate a)
                         (combiner (term a) acc)
                         acc))))
  (accumulate-iter a null-value))

(out (accumulate-rec + 0 identity 1 inc 1000 (lambda (x) (<= x 100))))
(out (accumulate-itr + 0 identity 1 inc 1000 (lambda (x) (<= x 100))))
; 5050
