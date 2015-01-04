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

b. Give a formula for the total number of pushes.

    | `n` | `S(n)` |
    |---|---|
    | 1 | 16 |
    | 2 | 72 |
    | 3 | 128 |
    | 4 | 240 |
    | 5 | 408 |
    | 6 | 688 |
    | 7 | 1136 |
    | 8 | 1864 |
    | 9 | 3040 |
    | 10 | 4944 |

    We assume the formula to be: `S(n) = a*S(n-1) + b*S(n-2) + k`

    Then the following equations must hold:

    * `S(4) = 128*a + 72*b + k = 240`
    * `S(5) = 240*a + 128*b + k = 408`
    * `S(6) = 408*a + 240*b + k = 688`

    Then we can compute the only solution:

    * `a = 1`
    * `b = 1`
    * `k = 40`

    Therefore:

    * `S(2) = 72`
    * `S(3) = 128`
    * `S(n) = S(n-1)+S(n-2)+40` for `n >= 4`

    Since `S(n) = a*Fib(n+1)+b`, we know that:

    * `S(n-1) = a*Fib(n) + b`
    * `S(n) = a*Fib(n+1) + b`
    * `S(n+1) = a*Fib(n+2) + b`
    * `S(n+1) = S(n) + S(n-1) + 40 = a*Fib(n+2) + b`

    Therefore `b = -40`, `a = 56`, `S(n) = 56*Fib(n+1) - 40` for `n >= 2`.
