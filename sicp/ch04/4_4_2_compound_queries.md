Just describe what I can understand so far.

"and" queries are implmeneted in two steps.
Suppose "A" is the first condition, the pattern matcher
will be executed against it and produce a new frame,
and this new frame will be fed to the second condition,
and the pattern matcher will be called again.

Handling compound queries can be very slow.
Though systems for handling only simple
queries are quite practical.

"not" is implemented by pattern matching its subcondition,
and those that meets the inner condition will be filtered out.
(there is some subtle issues regarding "not", but let's don't worry
about it for now)

"lisp-value" is just a filter on the stream.
