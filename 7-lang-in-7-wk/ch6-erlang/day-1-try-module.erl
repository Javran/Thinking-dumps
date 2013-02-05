% this file is supposed to be used as a module
%     this neither shebang nor run permission is set.
% use 'day-1-try-load-module' to test this module

-module('day-1-try-module').
-export([module_test/0]).
-export([writeln/1]).

module_test() ->
	io:format("function from module~n").

writeln(X) ->
	io:format("~s~n",[X]).
