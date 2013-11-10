(load "../common/utils.scm")
(load "../common/test-utils.scm")

; deque abstraction:
; constructor:
;   make-deque
; predicate:
;   empty-deque?
; selector:
;   front-deque
;   rear-deque
; mutator:
;   front-insert-deque!
;   rear-insert-deque!
;   front-delete-deque!
;   rear-delete-deque!


; actually the front-ptr is always the data container
;   and rear-ptr keeps pointing to the last pair of
;   that container (if possible)
(define (make-deque)
  (cons nil nil))

(define front-ptr car)
(define rear-ptr cdr)

(define set-front-ptr! set-car!)
(define set-rear-ptr! set-cdr!)

(define empty-deque? (compose null? front-ptr))

(define front-deque (compose car front-ptr))
(define rear-deque (compose car rear-ptr))

(define (front-insert-deque! q i)
  (let ((pair (cons i nil)))
    (if (empty-deque? q)
      (begin
        (set-front-ptr! q pair)
        (set-rear-ptr! q pair))
      (begin
        (set-cdr! pair (front-ptr q))
        (set-front-ptr! q pair)))))

(let ((dq (make-deque)))
  (for-each ((curry2 front-insert-deque!) dq)
            '(3 2 1))
  (out (front-ptr dq))
  )




(end-script)
