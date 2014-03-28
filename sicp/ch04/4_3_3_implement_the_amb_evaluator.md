The main difficulty for implementing `amb` evluator
is that the evaluator has a change of reaching a dead end,
in which case the evluation must backtrack (might have to cancel
some side effects).
