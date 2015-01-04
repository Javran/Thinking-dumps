a. Give a formula in terms of `n` for the maximum depth of the stack
required to compute `Fib(n)` for `n >= 2`.

    Results:

        ec-repl> (fib 4)
        3
        number-pushes = 240
        max-depth     = 23
        ec-repl> (fib 10)
        55
        number-pushes = 4944
        max-depth     = 53
        ec-repl> (fib 14)
        377
        number-pushes = 34120
        max-depth     = 73

    Formula: `maximum-depth(n) = 5*n+3`
