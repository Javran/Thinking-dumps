Alyssa\'s implementation is less efficient simply
because that it delays the execution of
`execute-sequence` to runtime.
That is, the analysis can proceed only if
we can provide a proper `env`, this could
only happen at runtime.
