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
