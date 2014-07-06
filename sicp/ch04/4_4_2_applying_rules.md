# Applying rules

To apply a rule, first use ordinary pattern matching
to see if any assertion can be applied.

And then attempt to unify the query pattern with the conclusion
of each rule.

And successful match produces frames.
(I guess frames are just equivalent to "some informations about binding variables"
in the high level idea)

## Simple queries

* pattern matcher deals with matching against all assertions
* unifier works with rules

(For now I still don't understand what does it mean,
maybe I'll get it once looking at the implementation)

Both pattern matching and unification produces streams,
and we combine them together to have all the possible answers.

## The query evaluator and the driver loop

(Well, pretty sure things are mixed together now,
the relationship between `qeval` and `eval`,
and what a driver loop can do, don't have to
be explained here.)
