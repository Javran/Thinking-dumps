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

(define (install-amb-ramb)

  (define (analyze-ramb exp)
    ;; the only difference: `cprocs` is shuffled
    (let ((cprocs (shuffle (map amb-analyze (amb-choices exp)))))
      (lambda (env succeed fail)
        (define (try-next choices)
          (if (null? choices)
              (fail)
              ((car choices)
               env
               succeed
               (lambda ()
                 (try-next (cdr choices))))))
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
