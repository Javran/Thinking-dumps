;; ==== modified version of the stack ====
(define (empty-stack)
  (vector
   '()
   ;; store some metadata
   ;; to help keep track of
   ;; some statistics
   '((number-pushes 0)
     (max-depth     0)
     (current-depth 0))))

;; modify a value in an alist using `proc`
;; the old value must exist
;; usage: ((modify-assoc key proc) alist)
(define (modify-assoc key proc)
  (lambda (alist)
    (let ((old-val (cadr (assoc key alist))))
      `((,key ,(proc old-val))
        ,@(del-assoc key alist)))))

(define (stack-modify-meta! st proc)
  (vector-modify! st 1 proc))

(define (stack-meta-get st key)
  (cadr (assoc key (vector-ref st 1))))

(define (stack-push! st e)
  (vector-modify!
   st 0
   (lambda (stack)
     (cons e stack)))
  ;; do statistics
  (stack-modify-meta!
   st (modify-assoc 'number-pushes add1))
  (stack-modify-meta!
   st (modify-assoc 'current-depth add1))
  (stack-modify-meta!
   st
   (modify-assoc
    'max-depth
    (lambda (old-max-depth)
      (let ((cur-depth (stack-meta-get st 'current-depth)))
        (max cur-depth old-max-depth))))))

(define (stack-pop! st)
  (vector-modify! st 0 cdr)
  ;; do statistics
  (stack-modify-meta!
   st (modify-assoc 'current-depth sub1)))
(define (stack-top st)
  (car (vector-ref st 0)))

(define (stack-empty? st)
  (eq? '() (vector-ref st 0)))

(define (stack-print-statistics st)
  (format
   #t
   "number-pushes = ~A~%~
    max-depth     = ~A~%"
   (stack-meta-get st 'number-pushes)
   (stack-meta-get st 'max-depth)))
