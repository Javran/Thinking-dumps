# Object System

Factor has an object system built on tuples.

* `TUPLE: <class-name> <slot-name-1> <slot-name-2> ...` declares a class.
* `<class-name> new` creates an object of that class (and push it to the stack)
* Slot operations
      - say the slot name is `foo`
      - `foo>>` reads the slot value
      - `>>foo` sets the slot value (of an object)
      - `change-slot` apply a quoted code block to the slot value of an object
