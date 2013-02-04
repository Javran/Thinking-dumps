#!/usr/bin/env escript

% This is a comment


% hint: 
% excerpted from erlang's doc:
%     On a Unix system you can view the manual pages from the command line using
%         erl -man <module>

main(_) -> 
	io:format("~w~n", [2+2]),
	% prints '4'
	io:format("~w~n", [2+2.0]),
	% prints '4.0'
	io:format("~w~n", ['string']),
	% prints 'string'
	io:format("~w~n", ["string"]),
	% prints char value list
	% very similar to prolog :)
	io:format("~w~n", [[1,2,3]]),
	% we could see from the doc that '~p' will attempt to print readably 
	% while '~w' will not
	io:format("~p~n", [[72,97,32,72,97,32,72,97]]),
	% "Ha Ha Ha"
	io:format("~w~n", [[72,97,32,72,97,32,72,97]]),
	% print values
	% following line will cause problem, because of type mismatching
	%     io:format("~w~n", [4+"string"]),
	% like prolog, 'variable' is an atom while 'Variable' will be a variable
	% so it is reasonable that the following line will not work:
	%     variable = 4,
	% but the following link will
	Var = 1,
	io:format("~w~n", [Var]),
	% prints '1'
	% variables is not allowed to re-define/assign:
	% so the following line causes error:
	%     Var = 2,
	io:format("~n",[]).
