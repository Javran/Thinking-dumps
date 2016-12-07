# 1 Notion of Computation

- An instance of `Arrow` is of the form `arrow i o`, where `i` represents the input type
  and `o` output type.

- any `Arrow` should satisfy `Category`. In other words, arrows are composable: there is
  a way to express the notion of "no operation" by using "id". and we can chain arrows together
  to produce new arrows

- additionally, any arrow is capable of running arbitrary pure functions, by using `arr :: Arrow a => (i -> o) -> a i o`.

- there are `first` and `second` that acts on part of an arrow's input and output. the ability
  to ignore some part of input / keep some part of input intact is important: it allows us to
  build up an arrow network that performs the computation we want.
