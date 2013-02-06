#!/usr/bin/env escript

number(one) 	-> 1;
number(two) 	-> 2;
number(three) 	-> 3.

main(_) -> 
	c:c(u),
	u:wl(number(one)),
	u:wl(number(two)),
	u:wl(number(three)),
	% the line below will not work
	% u:wl(number(four)),
	ok.
