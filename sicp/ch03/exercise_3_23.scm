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
      ; for empty deque, simply update
      ;   front & rear pointers to be the same
      (begin
        (set-front-ptr! dq node)
        (set-rear-ptr! dq node))
      ; else, we link new node with current front
      ;   and then update the front pointer
      (begin
        (set-next-node! node (front-ptr dq))
        (set-prev-node! (front-ptr dq) node)
        (set-front-ptr! dq node)))))

(define (rear-insert-deque! dq item)
  (let ((node (make-node item)))
    (if (empty-deque? dq)
      ; similiar to that of front-insert-deque!
      (begin
        (set-front-ptr! dq node)
        (set-rear-ptr! dq node))
      (begin
        (set-prev-node! node (rear-ptr dq))
        (set-next-node! (rear-ptr dq) node)
        (set-rear-ptr! dq node)))))

(define (front-delete-deque! dq)
  (if (null? (next-node (front-ptr dq)))
    ; if nothing remains, set all pointers to null
    (begin
      (set-front-ptr! dq nil)
      (set-rear-ptr! dq nil))
    ; front pointer go forward,
    ;   and disconnect the previous front node
    (begin
      (set-front-ptr! dq (next-node (front-ptr dq)))
      (set-prev-node! (front-ptr dq) nil))))

(define (rear-delete-deque! dq)
  (if (null? (prev-node (rear-ptr dq)))
    ; similiar to that of front-delete-deque!
    (begin
      (set-front-ptr! dq nil)
      (set-rear-ptr! dq nil))
    (begin
      (set-rear-ptr! dq (prev-node (rear-ptr dq)))
      (set-next-node! (rear-ptr dq) nil))))

(define empty-deque? (compose null? front-ptr))
(define front-deque (compose val-node front-ptr))
(define rear-deque (compose val-node rear-ptr))

(define (for-each-deque f dq)
  (if (empty-deque? dq)
    'done
    (let loop ((cur (front-ptr dq)))
      (if (null? cur)
        'done
        (begin
          (f (val-node cur))
          (loop (next-node cur)))))))

(let ((dq (make-deque)))
  (define (output-deque)
    (for-each-deque
      (lambda (x)
        (display x) (display #\space))
      dq)
    (newline))

  (for-each ((curry2 front-insert-deque!) dq)
            '(5 4 3 2 1))
  (for-each ((curry2 rear-insert-deque!) dq)
            '(6 7 8 9))
  (output-deque)
  )

(end-script)
