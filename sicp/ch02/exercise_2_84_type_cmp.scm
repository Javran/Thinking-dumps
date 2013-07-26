(define (higher-type? a b)
  ; the type tower, higher types are always placed in front of lower types
  (define type-tower
    '(complex real rational integer))

  ; find index of a symbol
  (define (find-index x ls)
    (let ((tail (member x ls)))
      (if tail
        (- (length ls) (length tail))
        #f)))

  ; comparison
  (< (find-index a type-tower)
     (find-index b type-tower)))

(define (test-type-cmp)
  (define (test testcase)
    (if (not (equal? (apply higher-type? (car testcase))
                     (cdr testcase)))
      (error "Test failed")))
  (for-each 
    test
    '(((complex real)   . #t)
      ((real real)      . #f)
      ((real rational)  . #t)
      ((rational real)  . #f)
      ((real integer)   . #t)
      ))
  (out "Test passed"))

; uncomment next line to see the test
; (test-type-cmp)
