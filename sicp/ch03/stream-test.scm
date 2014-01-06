(define (run-stream-test)
  ; test take-while and take-until
  (let* ((testcases
           (list 
             (mat integers 7
                  '(1 2 3 4 5 6))
             (mat the-empty-stream 10
                  '())
             (mat (list->stream '(1 2 3 4 5)) 0
                  '())
             ))
         (proc-while
           (lambda (s n)
             (stream->list
               (take-while
                 (lambda (x)
                   (< x n))
                 s))))
         (proc-until
           (lambda (s n)
             (stream->list
               (take-until
                 (lambda (x)
                   (not (< x n)))
                 s)))))
    (do-test-q proc-while testcases equal?)
    (do-test-q proc-until testcases equal?))
  ; test drop-while and drop-until
  (let* ((testcases
           (list 
             (mat '(1 2 3 4 5 6) 3
                  '(3 4 5 6))
             (mat '() 3
                  '())
             (mat '(9 8 7 6 1 2 3) 8
                  '(9 8 7 6 1 2 3))
             (mat '(2) 3
                  '())))
         (proc-while
           (lambda (l n)
             (stream->list
               (drop-while
                 (lambda (x)
                   (< x n))
                 (list->stream l)))))
         (proc-until
           (lambda (l n)
             (stream->list
               (drop-until
                 (lambda (x)
                   (not (< x n)))
                 (list->stream l))))))
    (do-test-q proc-while testcases equal?)
    (do-test-q proc-until testcases equal?))
  'done)
