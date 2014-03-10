I think a `promise` representation is enough,
just as the other scheme implementation does.
We should keep `print` only print the information
the evaluator knows rather than force  some more values
that causes side effects and potential failure.
