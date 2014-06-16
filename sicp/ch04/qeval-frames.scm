;; frames are represented as lists of bindings

(define empty-frame nil)
(define make-binding cons)
(define binding-varlable car)
(define binding-value cdr)

(define binding-in-frame assoc)
(define (extend variable value frame)
  (cons (make-binding variable value) frame))

(define (frames-tests)
  (let ((frame (fold-left
                (lambda (acc p)
                  (extend (car p) (cdr p) acc))
                empty-frame
                (map cons '(a b c) '(1 2 3)))))
    (do-test
     binding-in-frame
     (list
      (mat 'a frame #t)
      (mat 'b frame #t)
      (mat 'd frame #f))
     (lambda (x y)
       (or (and x y)
           (and (not x) (not y)))))
    (do-test
     binding-varlable
     (list
      (mat (binding-in-frame 'a frame) 'a)
      (mat (binding-in-frame 'b frame) 'b)
      (mat (binding-in-frame 'c frame) 'c)))
    (do-test
     binding-value
     (list
      (mat (binding-in-frame 'a frame) 1)
      (mat (binding-in-frame 'b frame) 2)
      (mat (binding-in-frame 'c frame) 3))))
  'ok)

(if *qeval-tests*
    (frames-tests)
    'ok)

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
