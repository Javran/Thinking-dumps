# Is Logic Programming Mathematical Logic?

* Not identical
* Decompose a computational problem into:
    * "what" is to be computed (using statements which are powerful enougth)
    * "how" this should be computed (controllable procedure,
    should be effective program that can be carried out by a computer)

## Infinite loops

The example:

* Define a rule: `(married Minnie Mickey)`
* We query: `(married Mickey ?who)`
* No answer because both the pattern matching and unfication fail
to find a solution
* Additional rule: `(rule (married ?x ?y) (married ?y ?x))`
* Infinite loop, the rule itself does not guarantee
a termination

## Problem with `not`

(Should I say that `and` is not commutative in this sense?)

Problem: two queries that looks the same mean different things

    (and (supervisor ?x ?y)
         (not (job ?x (computer programmer))))

    (and (not (job ?x (computer programmer)))
         (supervisor ?x ?y))

(I don't fully understand this, but I think
this is a problem caused by applying a `not` on something
that might have not yet bound to anything.)

Another serious problem: the mathematical statement `not P` means
`P` is not true. But here we are querying against a database,
and `not P` would mean `P` is not deducible. (Since it only
knows about assertion and rules, just like we cannot in general
find the inverse function for any inversible functions)

(For "closed world assumption", there is a link
in [Wikipedia](http://en.wikipedia.org/wiki/Closed_world_assumption))
