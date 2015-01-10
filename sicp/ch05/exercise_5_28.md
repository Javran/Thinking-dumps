# The recursive factorial machine

    ec-repl> (factorial 4)
    24
    number-pushes = 181
    max-depth     = 26
    ec-repl> (factorial 10)
    3628800
    number-pushes = 403
    max-depth     = 44
    ec-repl> (factorial 30)
    265252859812191058636308480000000
    number-pushes = 1143
    max-depth     = 104

# The iteractive factorial machine

    ec-repl> (factorial 4)
    24
    number-pushes = 120
    max-depth     = 35
    ec-repl> (factorial 10)
    3628800
    number-pushes = 324
    max-depth     = 83
    ec-repl> (factorial 30)
    265252859812191058636308480000000
    number-pushes = 1004
    max-depth     = 243

# Summary

\  | Maximum depth | Number of pushes
---|---|---
Recursive factorial | `f(x) = 3*x+14` | `f(x) = 37*x+33`
Iterative factorial | `f(x) = 8*x+3` | `f(x) = 34*x-16`
