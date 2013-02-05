#!/usr/bin/env escript

number(one) 	-> 1;
number(two) 	-> 2;
number(three) 	-> 3.

main(_) -> 
	io:format("~w~n",[number(one)]),
	io:format("~w~n",[number(two)]),
	io:format("~w~n",[number(three)]),
	% the line below will not work
	% io:format("~w~n",[number(four)]),
	io:format("~n",[]).
