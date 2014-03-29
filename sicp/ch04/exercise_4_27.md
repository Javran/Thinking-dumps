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

    I thought the output should be `0` but it turns out I was wrong:
    unlike what happens in Haskell, `w` gets partially evaluated when
    we are defining it, the delayed part is only the argument (i.e. `x`)
    itself. The correct answer would be `1` here.

* response #2

    The observation is that the `id` is a identity procedure
    with side effects (on `count`).
    So the return value of `(id (id 10))` should be the same
    as just evaluate `10` itself.

* response #3

    The `set!` gets evalulated twice because of the two procedure
    application, so the value of `count` increases by 2.
