#!/usr/bin/env escript

% This is a comment


% hint: 
% excerpted from erlang's doc:
%     On a Unix system you can view the manual pages from the command line using
%         erl -man <module>

main(_) -> 
	c:c(u),
	try_6_2_2(),
	try_6_2_3(),
	try_6_2_4().

% 6.2.2 basic
try_6_2_2() -> 
	u:wl(2+2),
	% prints '4'
	u:wl(2+2.0),
	% prints '4.0'
	u:wl('string'),
	% prints 'string'
	u:wl("string"),
	% prints char value list
	% very similar to prolog :)
	u:wl([1,2,3]),
	% we could see from the doc that '~p' will attempt to print readably 
	% while '~w' will not
	HaHa = [72,97,32,72,97,32,72,97],
	io:format("~p~n", [HaHa]),
	% "Ha Ha Ha"
	io:format("~w~n", [HaHa]),
	% print values
	% following line will cause problem, because of type mismatching
	%     u:wl(4+"string"),
	% like prolog, 'variable' is an atom while 'Variable' will be a variable
	% so it is reasonable that the following line will not work:
	%     variable = 4,
	% but the following link will
	Var = 1,
	u:wl(Var),
	% prints '1'
	% variables is not allowed to re-define/assign:
	% so the following line causes error:
	%     Var = 2,
	u:nl().

% 6.2.3 atom/list/tuple
try_6_2_3() -> 
	u:wl(red),
	Pill = blue,
	u:wl(Pill),
	% prints 'blue'
	u:wl([1,2,3]),
	% prints [1,2,3]
	u:wl([1,2,'three']),
	% [1,2,three]
	List = [1,2,3],
	u:wl(List),
	% [1,2,3]
	u:wl({one,two,three}),
	% tuple: one, two, three
	Origin = {0,0},
	io:format("origin: ~w~n", [Origin]),
	io:format("~p~n", [{name,"Spaceman Spiff"}]),
	io:format("~p~n", [{
				comic_strip, 
				{name, "Calvin and Hobbes"}, 
				{character, "Spaceman Spiff"} }]),	
	u:nl().

% 6.2.4 matching
try_6_2_4() ->
	Person = {
		person,
		{name, "Agent Smith"},
		{profession, "Killing programs"}},
	{person, {name, Name}, {profession, Profession}} = Person,
	io:format("Name: ~p~nProfession: ~p~n", [Name,Profession]),
	[Head|Tail] = [1,2,3],
	io:format("Head: ~p~nTail: ~p~n", [Head,Tail]),
	% 1,[2,3]
	[One, Two|Rest] = [Head|Tail],
	% One: 1, Two: 2, Rest: [3]
	io:format("One: ~w~nTwo: ~w~nRest: ~w~n", [One,Two,Rest]),
	% no match will be found
	%     [X1|Rest1] = [],
	pack_and_unpack(),
	u:nl().

pack_and_unpack() ->
	Origin = {1,2,3,4},
	{W,X,Y,Z} = Origin,
	io:format("Original data: ~w~n", [Origin] ),
	% "W:3" means W should take 3 bits
	All = <<W:3, X:3, Y:5, Z:5>>,
	io:format("Packed data: ~w~n", [All]),
	<<A:3, B:3, C:5, D:5>> = All,
	io:format("Unpacked data: ~w~n", [{A,B,C,D}]),
	ok.
