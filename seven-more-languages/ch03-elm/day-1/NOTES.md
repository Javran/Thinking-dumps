The syntax of Elm seems to have borrowed many from Haskell.
I'll only list few significant differences above:

* `value : type` where `type` is the type signature, and `::` are list "cons"

* multi-way-if is supported without giving any extra thing

* `\` for switching to a new line, which is useful in REPL

* Record type, you can write `{ fieldName1 = value1, fieldName2 = value2 }`,
  whose type will then be `{ fieldName1 : type1, fieldName2 : type2 }`

* Field accessors are polymorphic: `.x` can be applied to anything
  that has a field called `x`

* Prefer "pipe operators" ( `|>` and `<|` ) for function composition
