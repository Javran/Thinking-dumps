#!/usr/bin/env escript

main(_) -> 
	c:c(u),
	Numbers = [1,2,3,4],
	u:ws("Numbers:"),
	u:wl( Numbers ),

	u:ws("Print each element:"),
	Print = fun(Number) ->
			io:format("~p~n", [Number]) end,
	lists:foreach(Print, Numbers),

	u:ws("Apply +1 on each element:"),
	IncOne = fun(N) ->
			N + 1 end,

	u:wl( lists:map(IncOne, Numbers) ),
	u:wl( my_map(IncOne, Numbers) ),

	u:ws("Filter out elements less than 3:"),
	LessThan3 = fun(X) -> X < 3 end,
	u:wl(lists:filter(LessThan3, Numbers)),

	u:ws("Test `any` and `all`:"),
	% are all of the elements in a given list meet the requirement?
	u:wl(lists:all(LessThan3, [0,1,2])),
	% true
	u:wl(lists:all(LessThan3, [0,1,2,3])),
	% false
	% is there any element that meets the requirement?
	u:wl(lists:any(LessThan3, [0,1,2,3])),
	% true
	u:wl(lists:any(LessThan3, [3,4,5])),
	% false
	% note their behavoirs on empty list:
	u:wl(lists:all(LessThan3, [])),
	% true
	u:wl(lists:any(LessThan3, [])),
	% false

	u:ws("Test `takewhile` and `dropwhile`:"),
	u:wl(lists:takewhile(LessThan3, Numbers)),
	% [1,2]
	u:wl(lists:dropwhile(LessThan3, Numbers)),
	% [3,4]
	AnotherNumbers = [1,2,1,4,1],
	u:wl(lists:takewhile(LessThan3, AnotherNumbers)),
	% [1,2,1]
	u:wl(lists:dropwhile(LessThan3, AnotherNumbers)),
	% [4,1]
	ok.

my_map(F, [H|T]) -> [F(H)| my_map(F, T)];
my_map(F, []) -> [].

