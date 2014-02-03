Installing `scan-out-defines` to `make-procedure` would be better.
Since `make-procedure` is a constructor and `procedure-body` is an
extractor. We don't have to scan the procedure body every time
we want it.
