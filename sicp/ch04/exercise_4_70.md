The let bindings in `add-assertion!` and `add-rule!` is used to make the program correct.

Take `(set! THE-ASSERTIONS (cons-stream assertion THE-ASSERTIONS))` as an example.
Since `cons-stream` is a special whose `cdr` portion is delayed but the effect of `set!`
is taken place immediately. the opearation will point the `cdr` portion of the stream
to this stream itself.

However, the intented effect of this opearation is to add a new item
(i.e. `asssertion`) into the old existing stream, not to make a infinite list.
