# 1 Notion of Computation

- An instance of `Arrow` is of the form `arrow i o`, where `i` represents the input type
  and `o` output type.

- any `Arrow` should satisfy `Category`. In other words, arrows are composable: there is
  a way to express the notion of "no operation" by using "id". and we can chain arrows together
  to produce new arrows

- additionally, any arrow is capable of running arbitrary pure functions, by using `arr :: Arrow a => (i -> o) -> a i o`.

- there are `first` and `second` that acts on part of an arrow's input and output. the ability
  to ignore some part of input / keep some part of input intact is important:
  it gives us ability to dispatch different part of the input to different arrow components,
  which can then allows us to build up large and complex arrow network from it.

- `Arrow` instances include `Kleisli` arrows. Any valid instance of `Monad m` gives rise
  to an `Kleisli` arrow of type `a -> m b`, in which `a` represents input and `b` output.

- Other examples of `Arrow` include:

    - `Auto`: automata that accept an input and then produce an output and change itself
      into a new state (since under the setting of functional programming, everything should
      be immutable, this usually means a function of `i -> (o,a)`, where `i` is the input
      and `o` the output. we additionally have something of type `a` from the output,
      which is the automata after accepting the input, meant to replace the old automata
      before accepting the input.

    - `MapTrans`: I'm not entirely sure what this does. If you ignore `newtype` wrappers,
      you'll see this is some sort of function post-composition being seen as Arrow.
      With `zipMap` and `unzipMap`, it becomes easier to figure out type-hole
      guided instance implementation of this. but still, no idea what's the
      motivation behind it...

- `functor`?

    There is no formal definition of what is a functor in the paper.
    Some part of the explanation just doesn't make sense to me.
    What exactly does it mean by saying "XX is a functor in argument YY"?
