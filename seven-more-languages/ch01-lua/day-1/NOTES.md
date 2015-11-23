## Concepts

**prototype style**

* Create instances
* Clone instance and customize it
* Form a chain of prototype-relation, in which messages can be passed
(e.g. calling a missing function for an instance leads to searching
with that function name along the chain and calling the first match)

## Lua Syntax

Flexible syntax, whitespaces and semicolons usually don't matter.

```lua
print "No time for love"

print
  "No time for love"

print "No time" print "for love"
```

Hm, no need to repeat about basic things here.
From now I'll only make notes about things I find interesting.
