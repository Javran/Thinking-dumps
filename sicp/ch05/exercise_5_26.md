a. some outputs from the evaluator:

        ec-repl> (factorial 4)
        24
        number-pushes = 169
        max-depth     = 10
        ec-repl> (factorial 10)
        3628800
        number-pushes = 379
        max-depth     = 10
        ec-repl> (factorial 30)
        265252859812191058636308480000000
        number-pushes = 1079
        max-depth     = 10

    So that the maximum depth is 10 regardless
    of the number of `n`.

b. Since the formula is a linear function,
we assume it to be `f(x) = a*x+b`. Then
we have the following table:

    x | f(x)
    ---|---
    4 | 169
    10 | 379
    30 | 1079

    Thus the formula is: `f(x) = x*35+29`
