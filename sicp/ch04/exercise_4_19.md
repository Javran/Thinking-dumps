* Follow the specification and we will see Ben is right
* It is unreasonable to mask the outter `a`.
* The problem here is the scope of two `a`s,
It's hard to tell which one is correct in general,
it depends on the real meaning of `a`.
We should consider the proper behavior rather than
given an ambiguous code and think about all the possible behaviors.
* I cannot devise such a way, consider
what will happen if we define: 
`(define a (+ b 1)) (define b (+ a 1))` simultaneously.
