I won\'t implement this exercise, because it will introduce more confusions
than the problem it will solve. It costs too much to implement lazy evluation
as a native support rather than an extension, and the behavior of an lazy evaluator
might disagree with eager ones. I insist on implementing `stream` and using `list->stream`
to do the trick rather than using things like `'(a b c)` which semantically suggests
a normal list.
