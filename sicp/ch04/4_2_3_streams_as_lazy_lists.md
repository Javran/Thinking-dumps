* Problem with special forms

    Have more control over evaluators, but is not first class object,
    cannot work well with other functions. (They have to reimplement
    similiar functionalities, like streams)

* lazy by default

    If the evaluator uses lazy evaluation by default, lists and streams
    will be identical.

    If we allows non-strict primitives, we can implement `cons` as
    one of the non-strict primitives.

    We can implement `cons` as a non-primitive, as long as the evaluator
    is lazy (Therefore apply arguments to `(define (cons x y) ...)` will not
    the evaluation of `x` and `y`).

    Advantage: lists and streams are identical, no more `delay` and `force` trick

    Disadvantage: more unpredictable, hard to answer when will an expression
    be evalautated.
