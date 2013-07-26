(load "../common/utils.scm")

; * we need to describe the linear relationship of types
;   so we'll be able to compare the type levels
; * after we come up with the comparator, we'll modify the apply-generic
;   for the final target:
;   * get a list of types
;   * pick up the highest type in the type list
;   * every argument needs to be raised to be of that type
;   * keep raising if we cannot find a desired procedure
;   * abort if we've reached the highest level and find no result

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
      )))

; uncomment next line to see the test
; (test-type-cmp)

(end-script)
