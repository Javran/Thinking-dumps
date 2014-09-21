;; modified version of the stack
(define (empty-stack)
  (vector
   '()
   '((number-pushes 0)
     (max-depth     0)
     (current-depth 0))))

;; modify a value in an alist using `proc`
;; the old value must exist
(define (modify-assoc key proc alist)
  (let ((old-val (cadr (assoc key alist))))
    `((,key ,(proc old-val))
      ,@(del-assoc key alist))))

(define (stack-push! st e)
  (vector-modify!
   st 0
   (lambda (stack)
     (cons e stack))))
(define (stack-pop! st)
  (vector-modify! st 0 cdr))
(define (stack-top st)
  (car (vector-ref st 0)))

(define (stack-empty? st)
  (eq? '() (vector-ref st 0)))
