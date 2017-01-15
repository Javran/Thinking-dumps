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

# 2 Special cases

So the biggest question that the paper never seems to anwser is
where are these laws for different kinds of Arrows come from and how
they interact with each other?

Also, I don't know how this laws can be useful except for maybe constructing proofs or refactoring codes.
Perhaps we shouldn't worry too much about these laws if we haven't seen the need of it...

- `ArrowApply`

    - The interface is `app :: forall a b. arrow (arrow a b, a) b`.
    - any valid arrow that has `ArrowApply` instance, is just a `Monad` in disguise.
      from the type signature of `app`, we can see it has the ability applying
      one input to another to produce a result. (For functions this is trivial,
      the story will become interesting when talking about other ArrowApply
      instances.)

- `ArrowChoice`

    - The interfaces are:

        - `left :: arrow i o -> arrow (Either i a) (Either o a)`
        - `right :: arrow i o -> arrow (Either a i) (Either a o)`
        - `(+++) :: arrow li lo -> arrow ri ro -> arrow (Either li ri) (Either lo ro)`

    - Allowing applying different arrows on different kind of inputs (marked by either `Left` or `Right`).
      Think about the usually semantics of `if` expression / statement in common languages:
      one branch of expression / statement is never touched.

- `ArrowLoop`

    - Interface: `loop :: arrow (i,d) (o,d) -> arrow i o`

    - might have something to do with `MonadFix` for the `Kleisli` implementation, not sure.

    - somehow it seems like `fix f = let x = f x in x`, but I'm not convinced: at least `f` uses its
      argument to form a loop, but what exactly does `trace` do?

    - See comments right above `counter` of `Automata.hs`.

# 3 Arrow notation

This part is basically talking about GHC support for arrow notations
and how they can be translated into normal Haskell code.

My thoughts on this: it might not be a good thing to include arrow notation as
one part of GHC: I feel this translation is more like compilation: the semantics
is preserved while he translated code looks far less like the original one,
and there are oppotunities of optimization - and all of this complication
have very little to do with Haskell itself.

The exercise of this part asks us to do some translation manually using rules presented.
But the result is never perfect: the translation rules want to be safe and keeps everything
while the actual code uses just part of it, and unnecessary pipelines accumulate as the
whole arrow network getting more complicated. Optimization can be done manually if
we know exactly what the arrow network is doing and rule out those unnecessaries.
Or automatically if using more sophisticated rules.

# 4 Examples

## 4.1 Synchronous circuits

(TODO)

## 4.2 Homogeneous functions

(TODO)

## 4.3 Combining arrows

I see no big problems from this part, arrow transformer reminds me of
monad transformers, in which we do something on certain level while
keep some other parts intact. In some sense arrow transformers are
doing something similar.
