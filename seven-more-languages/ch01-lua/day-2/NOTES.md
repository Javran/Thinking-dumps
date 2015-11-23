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

## Metatables

In Lua metatable is a way to change the default behavior of certain things.
For now we know three of them:

* `__tostring`: used when we want string representation of something (print)
* `__index`: lookup key
* `__newindex`: assign one new key-value pair

Lua provides two functions to get or set metatable: `getmetatable`, `setmetatable`

One way to create tables that has certain behaviors could be: create a function that
creates a fresh table, initialize all required fields and set a metatable to it.
It's just like ``constructors'' in a Object-Oriented sense.

## Special Syntax for Handling OO schemes

In Object-Oriented programming, we will have access to a special reference
(most of the time it's called `this` or `self`) inside methods.
In Lua we either write it explicitly (e.g. `function foo.bar(self,baz) ... end`) or
use a special `:`-notation (e.g. `function foo:bar(baz)`), these two things are equivalent.
(note that `self` will then be a special variable name inside any function defined with
`:`-notation -- it's introduced implicitly)
