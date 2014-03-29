Usually we use "thunk" to implement lazy evluation.

Basic ideas:

* procedure application related (answer the question of when to evaluate the argument)
* a delayed computation
* can be forced when needed
* memoization:
    + do memozization: call-by-need (more common, efficient)
    + don\'t: call-by-name
