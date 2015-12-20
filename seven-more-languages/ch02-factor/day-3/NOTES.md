# Object System

Factor has an object system built on tuples.

* `TUPLE: <class-name> <slot-name-1> <slot-name-2> ...` declares a class.
* `<class-name> new` creates an object of that class (and push it to the stack)
* Slot operations
      - say the slot name is `foo`
      - `foo>>` reads the slot value
      - `>>foo` sets the slot value (of an object)
      - `change-slot` apply a quoted code block to the slot value of an object

* Tuple construction

    * `<slot-1-val> <slot-2-val> ... <class-name> boa` (see `boa` helps)
    * the name of a constructor follows the conversion of being surrounded by `<` and `>`.
      note that this is not a special syntax.
    * `T{` and `}` also creates objects

        - `T{ key-1 value-1 key-2 value-2 ... }`
        - `T{ f slot-val-1 slot-val-2 ...}`
