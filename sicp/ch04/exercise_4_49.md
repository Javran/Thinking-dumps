Suppose we are generating a list of characters that matches regular expression
`A+B+`, when using `amb`, the enumeration will alway begin with the first possible solution
and then move to the second one.
In this case, the string generated will be `A`, `AA`, `AAA`, ..., which get stuck in the branch
of `A`s without ways to get out. So strings like `AB`, `AABBB` will never have a chance to appear
in the result, which is unexpected.
