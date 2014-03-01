The output:

    (define (w (id (id 10))))
    ;;; L-Eval input:
    count
    ;;; L-Eval value:
    1   ; response #1
    ;;; L-Eval input:
    w
    ;;; L-Eval value:
    10  ; response #2
    ;;; L-Eval input:
    count
    ;;; L-Eval value:
    2   ; response #3


Explanation:

* response #1

    Trivial output, no function application is involved,
    count is initialized to `0` so the result should be `0`.


* response #2

    The observation is that the `id` is a identity procedure
    with side effects (on `count`).
    So the return value of `(id (id 10))` should be the same
    as just evaluate `10` itself.

* response #3

    The `set!` gets evalulated twice because of the two procedure
    application, so the value of `count` increases by 2.
