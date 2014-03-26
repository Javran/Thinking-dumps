For `amb` expressions, changing the evaluation order will not have
big effects on how many results it will produce or the correctness of
our program.(Given that all its subexpression will be properly terminated,
and that the results it might produce is finite)
This should be guaranteed by the specification of `amb`,
since in my understanding, `amb` should exhaustively try all the possibilities,
so it is supposed to cancel the side effects that parsers cause on `*unparsed*` variable
and retry other combinations.

However, something will go wrong if the operands in a `list` are
evaluated in some different ways. Take `parse-simple-noun-phrase` as an example,
`(parse-word articles)` must be evaluated before we can evaluate `(parse-word nouns)`,
therefore evaluating this list in reversed order will not guaranteed the correctness
of our program.
