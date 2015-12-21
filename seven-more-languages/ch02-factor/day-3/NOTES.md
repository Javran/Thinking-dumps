# Object System

Factor has an object system built on tuples.

* `TUPLE: <class-name> <slot-name-1> <slot-name-2> ...` declares a class.
* `<class-name> new` creates an object of that class (and push it to the stack)
* Slot operations
      - say the slot name is `foo`
      - `foo>>` reads the slot value
      - `>>foo` sets the slot value (of an object)
      - `change-slot` apply a quoted code block to the slot value of an object
      - `foo<<` that has stack effect `( obj val -- )`
      - See "Slot accessors" of the built-in document

* Tuple construction

    * `<slot-1-val> <slot-2-val> ... <class-name> boa` (see `boa` helps)
    * the name of a constructor follows the conversion of being surrounded by `<` and `>`.
      note that this is not a special syntax.
    * `T{` and `}` also creates objects

        - `T{ key-1 value-1 key-2 value-2 ... }`
        - `T{ f slot-val-1 slot-val-2 ...}`

# Case Studies

* Cart example

    Nothing new here except some uses of slot accessors and with some uses of combinators
    the code looks a little more elegant.

* Fizz Buzz

    Factor has a dynamic type system, that makes things a little bit flexible.
    We have a trick in this example: if something is already a string (not a number),
    then that thing must have satisfied some conditions and being a result, so no further processing
    is necessary. With this trick in mind, we can see by chainning words, every word has a chance to
    examine the input and turn it into a result if the precondition is satisfied.

# Where to go from here

This part has nothing fancy. I'm not going to summarize it, check them out in the book.
