# Question a

The problem is, `application?` makes the assumption that 
everything else are well handled and it only checks if the given expression
is non-empty.

That will cause assignments (form `(set! <variable> <value>)`,
definitions (form `(define <sym> <value>)` or `(define <proc and args> <value>)`,
ifs (form `(if <pred> <conseq> [alter])`), ... to be recognized as applications.

Evaluating `(define x 3)` will cause the interpreter to look up a corresponding
value of symbol `define` in the environment and apply `(x 3)` as arguments to it.

# Question b

See `./exercise_4_2.scm`
