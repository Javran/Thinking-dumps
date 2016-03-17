## Mathematical definition of total function and partial function

See [Partial function](https://en.wikipedia.org/wiki/Partial_function).

In a word if a function is defined on all possible input, then
this function is a total function, otherwise we call it partial.

## ``The Intellectual Ascent to Agda''

- [slides](https://github.com/boostcon/cppnow_presentations_2013/blob/master/tue/intellectual_ascent_to_agda.pdf?raw=true)
- [reddit discussion](https://www.reddit.com/r/programming/comments/220i20/david_sankel_the_intellectual_ascent_to_agda_with/)
- [video](https://www.youtube.com/watch?v=vy5C-mlUQ1w)

So I watched the video, and the talk is basically talking about the same thing
that 2nd part of day 3 is trying to make: use mathematics as a tool for modeling programming problems,
but I find this point to be weak:

- I can't see the point of using Idris or Agda for modeling the problem. What's important is
  to come up with a mathematical model that is easy to reason about, and I don't think relating it
  to dependent types is persuasive if we are targetting at C++:

  - How can you make sure your modeling can be converted to C++ correct? Isn't it another level of complication?
  - If you use Idris or Agda for modeling, how can you persuade your coworkers about the correctness of your model?
    Do they have to learn about Agda or Idris, why not just create a mathematical model using mathematical tools?
    What if others want to modify the protocol / interface / whatever ?

- For people to work together, we have to first make agreement on designing principles, as C++ has been exist for
  a long time and is used for doing serious business, I'm sure they are many sophisticated designing principles to use,
  what's the advantage of using this "denotational design"?

## Source to `Nat` module

- [Nat(0.10.2)](https://github.com/idris-lang/Idris-dev/blob/v0.10.2/libs/prelude/Prelude/Nat.idr)

## A list of proof tactics available in Idris

[Tactics and Theorem Proofing](http://docs.idris-lang.org/en/latest/reference/tactics.html)

TODO: This seems to be deprecated, not sure about what's the recommended way of constructing proofs.
