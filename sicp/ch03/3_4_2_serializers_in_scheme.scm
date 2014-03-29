(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./concurrent.scm")

(define x 10)

(parallel-execute
  (lambda ()
    (set! x (* x x))
    )
    ;(let loop ()
    ;  (display 'a) (loop)))
  (lambda ()
    (set! x (+ x 1))
    ))
    ;(let loop ()
    ;  (display 'b) (loop))))

(out "Wait for a while and input anything to proceed...")
(read-char)

(out x)

(end-script)
