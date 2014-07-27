I don't think exercise 4.77 are providing a good solution to the real problem.

We've learned in exercise 4.64 that the order of the query matters,
And also exercise 4.37 and some following exercises are also suggesting
taking a different ordering of the same computation
might result in different performance.

I believe people who write the query are responsible for its correctness and efficiency.
And these people MUST have a good understanding about what's going on in the system
in order not to provide a wrong or inefficient query.

I think lacking knowledge of this system is the real cause of this kind of problems.
But this exercise are suggesting us to essentially guess where should be the correct
position of a subquery, which I think is an ugly hack and does not solve the problem
at all. Additionally, I can't persuade myself
about the correctness of this extended system.

In exercise 4.67, we have actually implemented a loop detector
to help people using this system to troubleshoot. And I consider this
as a partial solution to the real problem.

My solution to this exercise is simple: making people using this system understand
the system itself, especially we should make them aware that the order matters and
try not to use a variable that has not yet been constrained too early. We need to make
good use of the tools like loop detector to find the problematic query.
