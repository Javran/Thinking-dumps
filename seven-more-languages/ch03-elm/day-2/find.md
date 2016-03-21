# Examples of different signals available in Elm

- `Window`: [dimensions / width / height](http://package.elm-lang.org/packages/elm-lang/core/2.1.0/Window)
- `Mouse`: [position / isDown / clicks](http://package.elm-lang.org/packages/elm-lang/core/2.1.0/Mouse)

# The relationship between `lift` and signals

`lift` seems to be deprecated, now the term is `map`.
Signal is a functor, that means when you have a regular function that deals with
values, Signal allows this function to also work on values under some Signal context
in a consistent way.

# A signal that fires every second

[Time.every](http://package.elm-lang.org/packages/elm-lang/core/2.1.0/Time#every)
