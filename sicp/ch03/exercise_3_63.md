If we use `(sqrt-stream x)` instead of `guess` in the
original implementation, we are creating an entirely new
stream over and over again. The advantage of using stream to
reuse the previous result will be lost.

For example, observation of `(sqrt-stream x)` would be like:

1. `(cons-stream 1.0 <?>)`
2. `(cons-stream 1.0 (stream-map ... (sqrt-stream x)))`
3. ...

Comparing with the original version:

1. `(cons-stream 1.0 <?>)`
2. `(cons-stream 1.0 (stream-map ... (cons-stream 1.0 <?>)))`
3. ...

If we do not use memoization, these two versions will not have
much difference, simply because that we have to redo all computations
instead of reusing previous results.
