The driver loop has some strange properties,
`try-again` now becomes a special symbol means
to retry the previous computation and get something else back.

Personally I think this behavior cannot be implemented
as an extension, the control flow is somehow weird for this evaluator.
In addition, I think we should write
`(prime-sum-pair (amb 1 3 5 8) (amb 20 35 110))`
to make it explicit that this procedure is special
which takes `amb` and might produce multiple results when using `try-again`.
