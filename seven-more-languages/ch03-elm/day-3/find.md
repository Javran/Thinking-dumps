* Evan's blog post on the implementation of Pong in Elm: [Making Pong](http://elm-lang.org/blog/making-pong)

    Evan's Pong example is very similar to our Language Heads, the code consists of 4 main parts:

    - **Input**: related signal of user inputs. If we need multiple signals, we can use
      combinators from `Signal`

    - **Model**: the state of the whole application. Should at least keep everything
      necessary for the application to proceed. We usually define structured data type
      for this purpose.

    - **Update**: when input signal comes, we should be able to modify our model accordingly.
      This is usually done by `Signal.foldp` on input, and implement a step function that
      takes user input and current state, and calculates next state.

    - **View**: render model on the screen. Usually done by mapping over the signal
      of the application state using a render function that renders application's state
      as web page elements.

