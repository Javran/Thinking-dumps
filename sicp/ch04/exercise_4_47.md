If the subexpressions in the amb are evaluated from left to right,
there should be no problem. You might argue that `parse-word` might cause
side effects, however, `amb` will make sure every subexpression is
evaluated in an equivalent environment. (In other words, `amb`
takes the responsibility of cancelling some side effects caused by its
subexpressions.

However, if the order of expressions is changed, the program
might not terminate. This is because `parse-verb-phrase` will
first try to use `parse-verb-phrase` without any condition,
which causes infinite loop.
