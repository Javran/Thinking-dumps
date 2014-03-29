# Native implemtation

`delay` need to be implemented as syntactic sugar:

    (delay <exp>) => (lambda () <exp>)

`force` can just simply run it as a procedure

    (define (force delayed-object)
      (delayed-object))

# Memoization

store the result of evaluation to get rid of efficiency issues

    ; maybe it's just another name for `thunk`
    (define (memo-proc proc)
      (let ((already-run? false)
            (result false))
        (lambda ()
          (if (not already-run?)
            (begin (set! result (proc))
                   (set! already-run? true)
                   result)
            result))))

    (delay <exp>) => (memo-proc (lambda () <exp>))
    (define (force delayed-object)
      (delayed-object))
