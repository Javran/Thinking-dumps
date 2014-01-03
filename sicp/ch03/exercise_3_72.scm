(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(load "./exercise_3_70_common.scm")

; group consecutive equal numbers together
; e.g. (1 1 2 2 3 4 5 5) => ((1 1) (2 2) (3) (4) (5 5))
; s: the stream
; equ?: a binary that indicates if two elements are equal.
(define (group-stream s equ?)
  ; remove consecutive elements from head,
  ;   return a stream of lists (element occurrence)
  (if (stream-null? s)
    the-empty-stream
    (let loop ((cur-s s)
               (cur-g '())
               ; all elements are compared with this one
               (element (head s)))
      (cond ((stream-null? cur-s)
              ; the stream is empty,
              ;   group the previous elements together.
              (cons-stream
                cur-g
                the-empty-stream))
            ((equ? (head cur-s) element)
              ; the current head can be groupped
              ;   into the current one
              (loop (tail cur-s)
                    (cons (head cur-s) cur-g)
                    element))
            (else
              ; cannot be grouped together
              (cons-stream
                cur-g
                (group-stream cur-s equ?)))))))

; test `group-stream`
(let ((testcases
        (list (mat '(1 1 2 2 3 4 5 5)
                   '((1 1) (2 2) (3) (4) (5 5)))
              (mat '(1 2 3 4 5)
                   '((1) (2) (3) (4) (5)))
              (mat '(1 1 1 1 1)
                   '((1 1 1 1 1)))
              (mat '()
                   '())
              ))
      (proc
        (compose
          stream->list
          (lambda (x) (group-stream x =))
          list->stream)))
  (do-test proc testcases equal?)
  (newline))

(define sum-of-two-square-stream
  (let ((weight
          (lambda (x)
            (apply + (map square x)))))
    (stream-map
      ; attach the sum to the stream
      (lambda (x) (cons (weight x) x))
      (weighted-pairs integers integers weight))))

(define sum-of-two-in-three-diff-ways
  (stream-filter
    ; exactly 3 different ways
    (lambda (x)
      (= (length x) 3))
    ; group stream using the sum
    (group-stream
      sum-of-two-square-stream
      (lambda (a b)
        (= (car a) (car b))))))

(print-few 10 sum-of-two-in-three-diff-ways)

(end-script)
