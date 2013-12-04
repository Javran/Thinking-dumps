; take a look at:
; http://web.mit.edu/benmv/6.001/www/threads.txt
(define (parallel-execute . procs)
  (map
    (lambda (p)
      (create-thread #f p))
    procs))

; I think I should go with other scheme impls
;   that has documented APIs
