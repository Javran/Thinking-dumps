Not sure what to do exactly. The hint doesn't offer much help.

So far by experimenting, I know that if you write `test` blocks in a test suite,
each of them will be executed independently. To be more specific,
if you store some video on the server in one testcase, you cannot retrieve it
in any other different testcases.

However there are ways to setup testcases: [ExUnit.Callbacks](http://elixir-lang.org/docs/stable/ex_unit/ExUnit.Callbacks.html) contains the exact thing I'm looking for.

So let's just interpret this exercise as: writing out some testcases
for testing the behavior of our database, any usage of `setup` is recommended.
