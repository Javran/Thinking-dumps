## is Io language strong typing or weak typing ?

Task #1 in day-1-answer-code.io

Io language is strong typing.

## bool test for some values

Task #2 in day-1-answer-code.io

`0` is `true`

`""` is `true`

`nil` is `false`

## slots that a prototype supports

Task #3 in day-1-answer-code.io

use `slotNames` to grab a list of all available slots of a prototype

## operator `::=`, `:=`, `=`

Task #4 in day-1-answer-code.io

The answer is located [here](http://iolanguage.org/scm/io/docs/IoGuide.html#Syntax-Assignment).

Let me grab something from that site:

* `a ::= 1` is equal to `newSlot("a",1)`
* `a := 1` is equal to `setSlot("a",1)`
* `a = 1` is equal to `updateSlot("a",1)`

These operators' behaviors can be changed by overriding these methods

## these operators should be used under which kinds of situation?

My points of view:

* `=` should be used only if the slot is created
* `:=` will make a slot, can assign values to it as well
* `::=` does what `:=` does, and additionally creates a setter for the slot

