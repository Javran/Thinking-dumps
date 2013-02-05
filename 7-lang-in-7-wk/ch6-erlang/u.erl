% utilities: this module is a collection of 
%     frequent used functions

-module(u).
-export([writeln/1]).

writeln(Term) ->
	io:write(Term),
	io:format("~n").
