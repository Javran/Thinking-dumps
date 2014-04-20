(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")

;; shuffle a list
(define (shuffle xs)
  (let* ((xsv (list->vector xs))
         (len (vector-length xsv)))
    ;; swap two elements in the position i,j of xsv
    (define (swap i j)
      (let ((ai (vector-ref xsv i))
            (aj (vector-ref xsv j)))
        (vector-set! xsv i aj)
        (vector-set! xsv j ai)))

    ;; i = [0..len-2], swap i with j, where j = [i+1..len-1]
    ;; in addition, i + 1 <= len -1, therefore i <= len - 2
    (for-each
     (lambda (ind-begin)
       (swap ind-begin
             (random-range-in (add1 ind-begin) (sub1 len))))
     (list-in-range 0 (- len 2)))
    (vector->list xsv)))

;; remove the element at a given position
(define (remove-at xs ind)
  (cond
   ;; nothing to do with empty list
   ;; and negative index
   ((or (null? xs)
        (< ind 0))
    xs)
   ;; remove the head
   ((= ind 0)
    (cdr xs))
   (else
    (cons (car xs)
          (remove-at
           (cdr xs) (sub1 ind))))))

;; move the element at a given place to the head of the list
(define (move-to-head xs ind)
  ;; fetch the element
  (let ((val (list-ref xs ind)))
    ;; remove it from the list and then attach it
    ;; to the head of the list
    (cons val (remove-at xs ind))))

(define (install-amb-ramb)

  (define (analyze-ramb exp)
    ;; vectorize the list for random access
    (let ((cprocs (map amb-analyze (amb-choices exp))))
      (lambda (env succeed fail)
        (define (try-next choices)
          (if (null? choices)
              (fail)
              (let* ((choice-ind
                      (random-range-in 0 (sub1 (length choices))))
                     (choices1 (move-to-head choices choice-ind)))
                ((car choices1)
                 env
                 succeed
                 (lambda ()
                   (try-next (cdr choices1)))))))
        (try-next cprocs))))

  (define (test)
    'todo)

  (define handler
    (make-amb-handler
     `ramb
     analyze-ramb
     test))

  (ahandler-register! handler)
  'ok)

(install-amb-ramb)

(run-all-slot-tests)

;; maybe the randomness is not enough,
;; consider put `random` inside the analyze,
;; only do it on the fly, and as needed
(out (amb-eval-all `(list (ramb 1 2)
                          (ramb 3 4)
                          (ramb 'a 'b 'c)) (init-env)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
