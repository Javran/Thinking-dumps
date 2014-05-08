# Program vs. knowledge

* Imperative knowledge tells us how to do something

* Declarative knowledge tells use what is something

But they have something in common: the both require
a step-by-step method to tell a computer what to do
to solve a problem.

A high-level language will provide some substantial amount
of methodological knowledge that frees the user from concern with
some implementation details and allows the user to describe
a higher level of idea using these methodological knowledge.

# Unidirectional computation

Thinking about a black box that has some interfaces exposed.
Unidirectional computation works in a way
that you give some inputs on a fixed set of interfaces, and you get back
results on another fixed set of interfaces.

But there are computations that works differently.
For example, a constraint network will have less require on
which interfaces are inputs and which are the outputs.
User might just pick up an interface of the black box
(as long as this choice makes some sense), feed it with
values and get results back. The distinction between input interface
and output interface are not fixed.

# Nondeterministic programming

Nondeterministic program gives a way that
something happens simultaneously within different worlds.
We just gives all the possible inputs and all the possible
outputs will be returned.

# Logic programming

Unification: symbolic pattern matching.

In my understanding, logic programming allows us
to not just write down function but also describe
relations.

Basic idea: a "what is" fact can be used to solve a number of
different problems that would have different "how to" components.

Example: list append.

What is:

* for any list `y`, the empty list and `y` append to form `y`.
* for any `u`, `v`, `y`, `(cons u v)` and `y` append to form `(cons u z)`
if `v` and `y` append to form `z`.

How to:

* find a list `y` that appends with `(a b)` to produce `(a b c d)`.

    * `y = (c d)`

* find all `x` and `y` that append to form `(a b c d)`.

    * `x = (), y = (a b c d)`
    * `x = (a), y = (b c d)`
    * `x = (a b), y = (c d)`
    * `x = (a b c), y = (d)`
    * `x = (a b c d), y = ()`
