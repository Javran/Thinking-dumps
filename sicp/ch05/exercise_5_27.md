Here are some results from running the recursive factorial machine:

    ec-repl> (factorial 4)
    24
    number-pushes = 112
    max-depth     = 23
    ec-repl> (factorial 10)
    3628800
    number-pushes = 304
    max-depth     = 53
    ec-repl> (factorial 30)
    265252859812191058636308480000000
    number-pushes = 944
    max-depth     = 153


\  | Maximum depth | Number of pushes
---|---|---
Recursive factorial | `f(x) = 5*x+3` | `f(x) = 32*x-16`
Iterative factorial | `f(x) = 10` | `f(x) = 35*x+29`
