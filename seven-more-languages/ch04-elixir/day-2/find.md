## `elixir-pipes` GitHub project

[Project home page](https://github.com/batate/elixir-pipes)

All core implementations are in one single file: [pipe.ex](https://github.com/batate/elixir-pipes/blob/0ea5d743a9409dcaef1dec8f81ddb719a59a799f/lib/pipe.ex)
This module works by modifying the semantics of Elixir pipe: `Marco.unpipe` breaks the pipe apart, `Enum.reduce` applies transformation to each
of those "code segments" and finally `Marco.pipe` ties things up to make it back into one piece.

`pipe_with` is not an exception of this: it transforms a pipe so that the custom function is embeded into each element of the pipe,
so the custom function has a chance of consuming intermediate results and determine what to do next.

## The supported Elixir module attributes

* An introduction about Elixir module attributes can be found [here](http://elixir-lang.org/getting-started/module-attributes.html)
  which has frequently used attributes: `@moduledoc`, `@doc`, `@behavior`, `@before_compile`.

* A complete list of supported module attributes (those that has special meanings to Elixir) can be found [here](http://elixir-lang.org/docs/v1.0/elixir/Module.html)

## A tutorial on Elixir-style metaprogramming

There are series in Elixir online documents, namely:

* [Quote and unquote](http://elixir-lang.org/getting-started/meta/quote-and-unquote.html)
* [Macros](http://elixir-lang.org/getting-started/meta/macros.html)
* [Domain Specific Languages](http://elixir-lang.org/getting-started/meta/domain-specific-languages.html)

TODO: notes after reading them

## Elixir protocols

[Protocols](http://elixir-lang.org/getting-started/protocols.html), from what I can read and
understand, is one way to achieve polymorphism in Elixir
This is pretty much like the concept of interface in many other object-oriented programming
languages.

## What does `function_exported?` do?

[`function_exported?`](http://elixir-lang.org/docs/v1.0/elixir/Kernel.html#function_exported?/3)

Looks like a way to tell if user has access to a specific function from some module.
