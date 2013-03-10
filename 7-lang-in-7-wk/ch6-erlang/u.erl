% utilities: this module is a collection of 
%     frequent used functions

-module(u).
-export([ws/1, wl/1, nl/0, readlines/1]).

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

% please refer to:
%     http://stackoverflow.com/questions/2475270/how-to-read-the-contents-of-a-file-in-erlang
% for details about how to read all lines in erlang
readlines(FileName) ->
	{ok, Device} = file:open(FileName, [read]),
	try get_all_lines(Device)
		after file:close(Device)
	end.

get_all_lines(Device) ->
	case io:get_line(Device, "") of
		eof  -> [];
		Line -> [string:strip(string:strip(Line),both,$\n) | get_all_lines(Device)]
	end.
