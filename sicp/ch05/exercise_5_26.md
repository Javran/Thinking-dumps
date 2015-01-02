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
