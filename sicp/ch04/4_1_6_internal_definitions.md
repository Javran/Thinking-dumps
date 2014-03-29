I'm not very happy to see the contents of this section.
The following stuffs is more like complaints rather than notes.
Sorry about that.

# Mutural recursive procedures

I actually can't see what is the problem.
As long as we can guarantee that when a procedure is
evaluated, the environment contains all the definitions
needed, everything will be fine.

# Eliminating all the internal definitions

This is somehow not acceptable for me.

What if the procedure to be transformed look like:

    (lambda <var>
      (define a <e1>)
      (define b <e2>)
      <do something here...>
      (define a <e3>)
      <do something here...>)

The strategy described in book will fail to explain this situation,
you cannot make two definition of `a` available simultaneously.
(I know this is not a good practice,
but it doesn't mean we should ignore this case)

# Transformation

I think this transformation is stupid,
The only good thing is that it marks variables
as not initialized so we can get errors when
the initialization has flaw.
And one transformation does not guarantee to eliminate all
internal definitions.
Suppore we have:

    (lambda (a)
      (define (f b)
        (define a 1)
        (define b 2)
        <do something here...>)
      (define (g x)
        ...)
      ...)

We can only get rid of the "first layer" of internal definition,
what about the rest?

To solve this problem, we may make the transformation recursive,
but this time we have to write some case analysis to cover
all the possible places where internal definitions might occur.
This change will hurt the modularity.
