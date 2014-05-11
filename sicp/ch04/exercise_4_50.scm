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
                      ;; pick up one randomly
                      (random-range-in 0 (sub1 (length choices))))
                     (choices1
                      ;; move it to the head to be consumed
                      ;; by the following computation
                      (move-to-head choices choice-ind)))
                ((car choices1)
                 env
                 succeed
                 (lambda ()
                   (try-next (cdr choices1)))))))
        (try-next cprocs))))

  (define (test)
    (let ((env (init-env)))
      ;; for random generated output,
      ;; we can only examine that all the values are produced
      (do-test
       amb-eval-all
       (list
        (mat `(ramb 1 2 3) env `(1 2 3))
        (mat `(+ (ramb 1 2) (ramb 3 4)) env `(4 5 5 6))
        )
       (lambda (actual expected)
         (equal? expected (sort actual <)))))
    'ok)

  (define handler
    (make-amb-handler
     `ramb
     analyze-ramb
     test))

  (ahandler-register! handler)
  'ok)

(install-amb-ramb)

(run-all-slot-tests)

;; the following two results should be the same when sorted
;; except that first one might not be in a deterministic order
(out (amb-eval-all
      `(list (ramb 1 2)
             (ramb 3 4)
             (ramb 'a 'b 'c))
      (init-env)))

(out (amb-eval-all
      `(list (amb 1 2)
             (amb 3 4)
             (amb 'a 'b 'c))
      (init-env)))

;; How this can help with Alyssa's problem in exercise 4.49?

;; suppose we want to generate a list that contains only "a"s and "b"s

(define (run-demo-using amb-impl-symb)
  (out (take 5
           (amb-eval-all
            `(begin
               (define (demo1 n)
                 (if (= n 0)
                     '()
                     (cons (,amb-impl-symb 'a 'b) (demo1 (- n 1)))))
               (demo1 4))
            (init-env)))))

;; if we use "amb", the result will look like:
(run-demo-using 'amb)

;; from the result we can observe that "b" won't have any chance to
;; appear in the resulting list before "a". And this behavior might get stuck
;; if the length of the list is infinite.

;; on the other hand, if we allow candidates to be attempted in a random order,
;; we will have less chance to get stuck somewhere.
;; consider a modified version:
(run-demo-using 'ramb)

;; in this version, we don't always pick up the first candidate,
;; which somehow solves the problem of getting stuck
;; because of the recursive structure

(newline) (newline)
;; finally we have all the tools to solve this problem.

(load "./natural_language_common.scm")

(for-each
 (lambda (e)
   (pretty-print e) (newline) (newline))
 (stream-take
  5
  (amb-eval-stream
   (run-source-in-env
    `(begin
       (define (an-random-element-of xs)
         (if (null? xs)
             (ramb)
             (ramb (car xs)
                   (an-random-element-of (cdr xs)))))

       (define (parse-word word-list)
         (list (car word-list)
               (an-random-element-of (cdr word-list))))
       (parse '())
       ))
   (amb-init-env))))

;; now we can see the result is more interesting.
;; thanks to the randomness, every candidate will now get a chance
;; to show up in the generated sentence.

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
