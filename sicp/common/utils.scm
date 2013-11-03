(load-option 'format)

(define end-script
  (lambda ()
    (if (string=? (string microcode-id/operating-system) "unix")
      (%exit)
      (exit))))

; just follow the suggestion here:
; http://stackoverflow.com/questions/15552057/is-it-possible-to-implement-define-macro-in-mit-scheme/
; we should prefer syntax-rules over define-macro
;     so that we'll get clearer codes.
(define-syntax define-syntax-rule
  (syntax-rules ()
    ((define-syntax-rule (name . pattern) template)
     (define-syntax name
       (syntax-rules ()
         ((name . pattern) template))))))

; see:
; http://www.ps.uni-saarland.de/courses/info-i/scheme/doc/refman/refman_11.html#IDX1288
; in mit-scheme, `gensym` is called `generate-uninterned-symbol`
(define gensym generate-uninterned-symbol)

(define call/cc call-with-current-continuation)

(define out
  (lambda items
    (for-each 
      (lambda (x)
           (display x)
           (newline))
      items)))

(define pi 3.141592653589793)

; calculate time difference
; returns a pair (return value . time elapsed)
(define time-test
  (lambda (f . args)
    (let ((start-time (real-time-clock)))
      ; require the value immediately to force it
      start-time
      (let ((f-result (apply f args)))
        (let* ((end-time (real-time-clock))
               (diff-time (- end-time start-time)))
          ; force it
          diff-time
          (display "Time elapsed: ")
          (display diff-time)
          (newline)
        (cons f-result diff-time))))))

(define (identity x) x)
(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (random-range-in a b)
  (+ a (random (+ (- b a) 1))))
  ; random (b-a)+1 -> [0, (b-a)] + a -> [a, b] 

; take-iterate: make a finite list with 'len' elements
; the first one is 'seed', and others are generated by applying prev element to 'next'
(define (take-iterate next seed len)
  (if (= 0 len)
    '()
    (cons seed 
          (take-iterate next (next seed) (- len 1)))))

(define (gen-list from to step)
  (if (> from to)
    '()
    (cons from
          (gen-list (+ from step) to step))))

; construct a list [a..b]
(define (list-in-range a b)
  (gen-list a b 1))

(define enumerate-interval list-in-range)

(define average
  (lambda x
    (/ (apply + x) (length x))))

; return a function that drops the input and return x constantly
(define (const x)
  (lambda (y) x))

; use `pair?` to judge if a list contains any element is not straighforward
; so here I made an alias to make the code more friendly to readers
(define non-empty? pair?)

(define nil '())

(define (prime? n)
  (define (smallest-divisor n)
    (define (divides? a b)
      (= (remainder b a) 0))
    (define (square x) (* x x))
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n) ; impossible
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (+ test-divisor 1)))))
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (concat ls)
  (fold-right append nil ls))

(define (flatmap f ls)
  (concat (map f ls)))

(define concatmap flatmap)

; are a and b close enough?
(define (close-number? eps)
  (lambda (a b) 
    (< (abs (- a b)) eps)))

; compose unaries
; compose (...  h . g . f) x = ... $ h $ g $ f x
; => compose-inv (f . g . h ...) x
; => (compose-inv (g . h ...)) $ f x
(define (compose . procs)
  (define (compose-inv procs)
    (if (null? procs)
      identity
      (lambda (x)
        ((compose-inv (cdr procs)) ((car procs) x)))))
  (let ((procs-inv (reverse! procs)))
    (compose-inv procs-inv)))

(define (curry2 f)
  (lambda (a) (lambda (b) (f a b))))

(define (flip f)
  (lambda (a b) (f b a)))

(define (drop-while pred ls)
  (if (null? ls)
    '()
    (if (pred (car ls))
      (drop-while pred (cdr ls))
      ls)))

; recursively compare two lists
(define (rec-eq? equ?)
  (define (compare a b)
    (cond 
      ; both are nulls
      ((and (null? a) (null? b)) #t)
      ; only one of them is null
      ((or  (null? a) (null? b)) #f)
      ((and (pair? a) (pair? b))
        (and (compare (car a) (car b))
             (compare (cdr a) (cdr b))))
      ; both are not pairs
      ((and (not (pair? a))
            (not (pair? b)))
        (equ? a b))
      (else #f)))
  compare)

; take upto n elements from the list
(define (take n ls)
  (if (or (= n 0) (null? ls))
    nil
    (cons (car ls) (take (- n 1) (cdr ls)))))

'done
