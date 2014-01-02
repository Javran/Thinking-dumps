(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(load "./exercise_3_70_common.scm")

(define (cube-weight xs)
  (let ((i (car xs))
        (j (cadr xs)))
    (+ (* i i i)
       (* j j j))))

(define cube-sum-stream
  (weighted-pairs integers integers cube-weight))

; drop while `pred` holds
(define (drop-while pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (head s))
          (drop-while pred (tail s)))
        (else
          s)))

(define (find-consecutive-num s)
  (define (find-consecutive-num-aux s last-one)
    (cond
      ((null? s) the-empty-stream)
      (last-one
        ; last one = head
        (if (= (head s) last-one)
          (cons-stream
            last-one
            (find-consecutive-num-aux
              ; drop consecutive equal values
              (drop-while
                ((curry2 =) last-one)
                (tail s))
              ; and make a fresh start
              #f))
          ; else keep going
          (find-consecutive-num-aux
            (tail s)
            (head s))))
      (else
        (find-consecutive-num-aux
          (tail s)
          (head s)))))
  (find-consecutive-num-aux s #f))

(define ramanujan-numbers
  (find-consecutive-num
    (stream-map cube-weight cube-sum-stream)))

(print-few 6 ramanujan-numbers)
; (1729 4104 13832 20683 32832 39312)

(end-script)
