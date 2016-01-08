* In Elixir there is a concept called **destructing**, which is just pattern matching
  or unification, which checks the consistency of known values and declare and bind unknown ones

* **Imperative style but internally functional**, Elixir allows an imperative style of updating
  things, e.g. you can write `a = 1` then `a = a + 1`, but internally what happens is
  `a1 = 1` then `a2 = a1 + 1`. For now I'm good with it, maybe as we get more comfortable with
  this language, we'll think more about whether it's good or not.

* This is a programming language full of syntactic sugars. I'm not going to list them here.
  Nothing really looks significant to me.

* Think Elixir modules as executables. Elixir is a scripting language, and code gets interpreted
  at runtime. To make available a module, Elixir executes the code, and anything declared with
  `defmodule` is then accessible with proper module names.

## Exercise Notes

Here are some notes about the exercise and about Elixir language itself.

* Functions have to be defined in modules, top-level definitions are invalid.
* `def function_name (var)` does not work while `def function_name(var)` works. So make sure not to
  leave spaces between function name and the following parentheses.
* Unlike function definitions, which can only be defined in a module,
  you can create anonymous functions and bind it to a variable.
  But then there is no easy way of writing down recursive anonymous functions (you cannot refer to
  the function itself inside the function body)
* Use `x = <pattern>` so that the whole pattern can be bound to variable `x`. (e.g. `def foo(data={a,b})`)
* When doing pattern matching, remember to pin some variables so there won't be captured accidentally.
  (see: [Pin Operator](http://elixir-lang.org/getting-started/pattern-matching.html#the-pin-operator) )
