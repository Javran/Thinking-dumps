# Parallel and Concurrent Programming in Haskell

Codes and thoughts about this book when reading it xD

## Setup

I'll use `parconc-examples-0.3.5`, go to subdirectory `external` and run:

```shell
stack unpack parconc-examples-0.3.5
```

Note that you should use command `stack exec -- runghc <.hs file>` to run codes
under `src` directory (or of course using `ghc` to compile).
The cabal file `pcph-learn.cabal` is only responsible for pulling in
required packages.
You know how boring it is maintaining a long list of executables in `.cabal` files
so I'd opt out for doing that.
