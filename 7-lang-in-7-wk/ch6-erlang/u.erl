% utilities: this module is a collection of 
%     frequent used functions

-module(u).
-export([ws/1, wl/1, nl/0]).

% write string, together with newline, to stdout
ws(Term) ->
	io:format("~s~n", [Term]).

% write value to stdout, with newline.
%     if you do not need to print quotation mark,
%     use 'ws/1' instead.
wl(Term) ->
	io:format("~w~n", [Term]).

% newline
nl() -> io:format("~n").
