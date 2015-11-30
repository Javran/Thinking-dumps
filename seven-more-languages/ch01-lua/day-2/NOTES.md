## Tables

* Like JavaScript, in Lua we can use either dot-notation or square brackets
  `foo.bar` or `foo['bar']`. The latter is preferred usually when the key
  is a variable whose value cannot be determined statically.

* In Lua tables are used in place of arrays. A syntax called ``Table constructor'' is used for creating tables.

```lua
ice_cream_scoops = {
    "vanilla",
    "chocolate";

    sprinkles = true
}
```

if no explicit key is given, the corresponding values are indexed as if
the table is an array.

* `pairs(table)` creates an iterator for table key-value pairs

### Metatables

In Lua metatable is a way to change the default behavior of certain things.
For now we know three of them:

* `__tostring`: used when we want string representation of something (print)
* `__index`: lookup key
* `__newindex`: assign one new key-value pair

Lua provides two functions to get or set metatable: `getmetatable`, `setmetatable`

One way to create tables that has certain behaviors could be: create a function that
creates a fresh table, initialize all required fields and set a metatable to it.
It's just like ``constructors'' in a Object-Oriented sense.

### Special Syntax for Handling OO schemes

In Object-Oriented programming, we will have access to a special reference
(most of the time it's called `this` or `self`) inside methods.
In Lua we either write it explicitly (e.g. `function foo.bar(self,baz) ... end`) or
use a special `:`-notation (e.g. `function foo:bar(baz)`), these two things are equivalent.
(note that `self` will then be a special variable name inside any function defined with
`:`-notation -- it's introduced implicitly)

### Set table to `__index`

We can set `__index` of the metatable to be a table rather than a function.
If we do so, when a key is missing, we will try to look it up in that table instead.

Some experiment:

```lua
dictA = { a = 1, b = 2 }
dictB = { c = 3, d = 4 }
dictC = { e = 5 }
setmetatable(dictB, { __index = dictA })
setmetatable(dictA, { __index = dictC })

print(dictB.a)
print(dictB.b)
print(dictB.c)
print(dictB.d)
-- dictB, no "e" -> dictA, no "e", dictC, found
print(dictB.e)
```

This code snippet prints from 1 to 5. What's interesting is that
we have a chain of fallback: `dictA`, `dictB`, `dictC`, much like in
Object-Oriented Programming (or we'd better say its like prototype-based language),
if we cannot find some method, we'lll check its parent (its prototype) instead.

## Coroutines

Lua dosn't handle multithreading, but it uses coroutines instead.

Coroutines are not preemptive, and the program should explicit point out
when can the current task be safely paused.

`coroutine.create(func)` creates a coroutine that runs function `func`.
If the body of the function contains some calls to `coroutine.yield(v)`,
then the execution of the function will be paused at that point and `v` will be produced.
Also that the coroutine can be resumed in future by calling `coroutine.resume`
