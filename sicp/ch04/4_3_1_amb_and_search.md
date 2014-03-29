# `amb`

`(amb <e1> <e2> ... <en>)` means one of the value from `e1`, `e2`, ..., `en`.
Usually the underlying "search engine" will try them one by one as
this is the easiest way of implementing this feature. ("systematically search all
possible execution paths")

If no choice is given, i.e. `(amb)`, that just means a failure.

# `require`

`(require <p>)` puts an constraint on the choices, note here only `<p>`
is passed as an argument, so I think `amb` might keeps some informations globally and
might not be thread-safe.

# nested `amb`

In addition, `(amb <e1> (amb <e2> <e3>) ...)` seems to be supported, that should
be equivalent to `(amb <e1> <e2> <e3> ...)`.
