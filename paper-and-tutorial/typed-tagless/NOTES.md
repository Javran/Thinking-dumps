## 3.5 Relating initial and final typed tagless encodings

* if the encoding is tight, that means only well-typed objects are representable,
  therefore no need for writing a type checker of our own.

* the correspondence between initial and final encoding can be shown
  by converting one into another. For final encoding, we just need to have
  another "Symantics" that feeds arguments to the constructors of initial encoding;
  for initial encoding, we are just doing case analysis to destruct contained values
  and hand them over to the final "constructors"

# 4

## 4.1 Typed compliation

Basically we need something like `Data.Typeable` to have a type representation at value level
(as the type level is eventually "erased" when running a Haskell program). Here `Typ.hs` is the tool we use:

- the type equivalence is established through a observation: "forall c. c a -> c b",
  if we can find a function that fits this type, then that function serves both as a proof
  and a way to convert from `a` to `b` (we can let `c ~ Identity` so it will become `Identity a -> Identity b`).

- more equivalence relations can be established by composing proofs together. the we have the `newtype` trick
  to declare "type level functions" to assist our proofs.

- now that we can:

    - store type info at value level (through equivalence relations)
    - construct type info at value level
    - safe type cast (by using the function that establishes the equivalence relation)

- by using these facilities, we can write our own type-checkers.
  it's true what we have very limited type level information, but equivalence relations
  are on value level (as functions) to work with.

# 4.2 Typed formatting

- a language for describing formatting.

- by interpreting it in different ways, we can use the "FormattingSpec"
  to print or parse typed terms.

- for this part the implementation looks tricky to me, but I get the idea
  of using continuations through interpretation to build up a function
  that accepts arguments as required to get to the final result.

# 4.3 Linear and affine lambda-calculi

TODO

# 4.4 Call-by-name, call-by-value, call-by-need

TODO

# 4.5 Typed ordinary and one-pass CPS transforms

- CPS transformation (Fischer & Plotkin, call-by-value)

- CPS transformation (Danvy & Filinski)
    - "administrative indices": to my knowledge these are structures that looks trivial
      (e.g. `((\x -> x 1) (\y -> <...>))` is just `<...>` with `y` properly replaced by `1`)
    - Danvy & Filinski 's version claims to remove "administrative indices"

- CPS transformation can be done multiple times, this demonstrates
  the composability of interpreters. I'm not going to dig into this
  as I think the result itself could be over-complicated.

- CPS transformation does not preserve type (just preserves "typing"),
  and we use type family (type functions) to work around it.
