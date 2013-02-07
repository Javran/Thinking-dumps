#!/usr/bin/env escript

main(_) -> 
	c:c(u),
	u:wl(testIf(-1)),
	% negative
	u:wl(testIf(1)),
	% positive
	u:wl(testIf(0)),
	% zero
	ok.


testIf(X) ->
	if
		X > 0 -> positive;
		X < 0 -> negative;
		% we need to assign values for all conditions
		true -> zero
	end.
