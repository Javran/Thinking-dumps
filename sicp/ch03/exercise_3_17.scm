(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (count-pairs x)
  ; auxiliary function: given a pair and a list of visited items
  (define (count-pairs-mem x visited)
    (cond ((memq x visited) (cons 0 visited))
          ((pair? x)
            (let* ((visited1 (cons x visited))
                   ; make x as visited
                   (result1 (count-pairs-mem (car x) visited1))
                   ; accumulate result from its first element
                   (visited2 (cdr result1))
                   (count1 (car result1))
                   (result2 (count-pairs-mem (cdr x) visited2))
                   ; accumulate result from its second element
                   (visited3 (cdr result2))
                   (count2 (car result2)))
              ; put them together
              (cons (+ 1 count1 count2) visited3)))
          (else (cons 0 visited))))
  (car (count-pairs-mem x nil)))

; examples taken from ex 3.16:
(define x1 '(a b c))
(define x2
  (let ()
    (define u '(c))
    (define v (cons 'b u))
    (cons u v)))
(define x3
  (let ()
    (define u '(c))
    (define v (cons u u))
    (cons v v)))
(define x4
  (let ()
    (define u '(a b c))
    (set-cdr! (cddr u) u)
    u))

(for-each (compose out count-pairs) (list x1 x2 x3 x4))
; the output should all be 3 now.

(end-script)
