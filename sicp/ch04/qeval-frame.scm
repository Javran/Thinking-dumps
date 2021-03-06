;; frames are like environments,
;; represented as a list of pairs
;; where key and value are represented as a pair in scheme

;; we use "make-binding" to make key value pairs
;; use "extend" to extend an existing frame
;; and use "binding-in-frame" to retrieve a specified binding
;; from the frames
(define empty-frame nil)

(define make-binding cons)
(define (extend variable value frame)
  (cons (make-binding variable value) frame))

(define binding-variable car)
(define binding-value cdr)
(define binding-in-frame assoc)

(define alist->frame identity)

(define (qeval-frame-tests)
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
     binding-variable
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
    (qeval-frame-tests)
    'ok)

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
