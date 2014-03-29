# Metacircular

An evaluator that is written in the same language that it evaluates.

# `eval` and `apply`

## evaluate

1. evaluate all subexpressions
2. apply the first subexpression(the operator)
to the rest subexpression(the operands)

## apply

1. construct new environment by extending procedure's
environment part to include the formal parameters
2. evaluate the body in this new environment
