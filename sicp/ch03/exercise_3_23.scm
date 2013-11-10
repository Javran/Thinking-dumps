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

; building block: nodes: (cons <value> (cons <prev> <next>))
(define (make-node x)
  (cons x (cons nil nil)))
(define val-node car)
(define prev-node cadr)
(define next-node cddr)

(define (set-prev-node! n x)
  (set-car! (cdr n) x))
(define (set-next-node! n x)
  (set-cdr! (cdr n) x))

(define (make-deque)
  (cons nil nil))
(define front-ptr car)
(define rear-ptr cdr)
(define set-front-ptr! set-car!)
(define set-rear-ptr! set-cdr!)

(define (front-insert-deque! dq item)
  (let ((node (make-node item)))
    (if (empty-deque? dq)
      (begin
        (set-front-ptr! dq node)
        (set-rear-ptr! dq node))
      (begin
        (set-next-node! node (front-ptr dq))
        (set-prev-node! (front-ptr dq) node)
        (set-front-ptr! dq node)))))

(define (rear-insert-deque! dq item)
  (let ((node (make-node item)))
    (if (empty-deque? dq)
      (begin
        (set-front-ptr! dq node)
        (set-rear-ptr! dq node))
      (begin
        (set-prev-node! node (rear-ptr dq))
        (set-next-node! (rear-ptr dq) node)
        (set-rear-ptr! dq node)))))

(define empty-deque? (compose null? front-ptr))
(define front-deque (compose val-node front-ptr))
(define rear-deque (compose val-node rear-ptr))

(define (print-deque dq)
  (let loop ((n (front-ptr dq)))
    (if (null? n)
      'done
      (begin
        (out (val-node n))
        (loop (next-node n))))))

(let ((x (make-deque)))
  (for-each ((curry2 front-insert-deque!) x)
            '(5 4 3 2 1))
  (for-each ((curry2 rear-insert-deque!) x)
            '(6 7 8 9))
  (print-deque x)
  )

(end-script)
