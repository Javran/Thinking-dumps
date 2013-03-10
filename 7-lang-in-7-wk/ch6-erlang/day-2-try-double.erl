#!/usr/bin/env escript

double_all([]) -> [];
double_all([H|T]) -> [H+H|double_all(T)].

my_double_all(L) ->
	Double = fun(X) -> X + X end,
	lists:map(Double, L).

main(_) -> 
	c:c(u),

	Numbers = [1,2,3,4],
	u:ws("Numbers:"),
	u:wl(Numbers),
	u:ws("Double it:"),
	u:wl(double_all(Numbers)),
	u:wl(my_double_all(Numbers)),

	u:ws("Test list constructors"),
	u:wl([1|[2,3]]),
	% just like `1:[2,3]` in haskell
	% [1,2,3]
	u:wl([[2,3]|1]),
	% I don't know what it is, since '1' is not a list
	% [[2,3]|1]
	u:wl([[]|[2,3]]),
	% [[]|[2,3]]
	u:wl([1|[]]),
	% [1]
	ok.
